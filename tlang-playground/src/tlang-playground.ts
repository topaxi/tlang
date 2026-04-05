import { LitElement, PropertyValueMap, css, html } from 'lit';
import { customElement, query, state } from 'lit/decorators.js';
import { examples } from './examples';
import { TlangController } from './controllers/tlang-controller';
import { type DiagnosticMessage } from './controllers/tlang-controller';

import './components/t-button';
import './components/t-icon';
import './components/t-codemirror';
import './components/t-console';
import './components/t-diagnostics';
import './components/t-hir-pretty';
import './components/t-live';
import './components/t-menu';
import './components/t-select';
import './components/t-shortcuts';
import './components/t-split';
import './components/t-tabs';
import './components/t-toolbar';
import { type TCodeMirror } from './components/t-codemirror';
import { ConsoleElement, ConsoleMessage } from './components/t-console';
import { FlashElement } from './components/t-flash';
import { SelectElement } from './components/t-select';
import { SplitElement, SplitEvent } from './components/t-split';
import { compressSource, decompressSource } from './utils/lz';
import {
  type Runner,
  type JsHirPrettyOptions,
  type JsOptimizationOptions,
} from './tlang';
import { keyed } from 'lit/directives/keyed.js';
import { live } from 'lit/directives/live.js';
import { repeat } from 'lit/directives/repeat.js';
import { mediaQuery } from './decorators/media-query';
import { floating } from './directives/floating';

type OutputDisplay = 'ast' | 'hir' | 'jsast' | 'javascript';

function getHashParams() {
  return new URLSearchParams(window.location.hash.slice(1));
}

// Default source code is either the code provided via hashcode "source"
// compressed by lz-string or the "example" hashcode with the corresponding
// example file name. Default is the first example.
async function defaultSource() {
  let params = getHashParams();

  if (params.has('source')) {
    return await decompressSource(params.get('source')!);
  } else if (params.has('example')) {
    let exampleName = params.get('example') ?? '';
    if (exampleName in examples) {
      return examples[exampleName];
    }
  }
  return examples[Object.keys(examples)[0]];
}

const displayLabels = {
  ast: 'AST',
  hir: 'HIR',
  jsast: 'JS AST',
  javascript: 'JavaScript',
} satisfies Record<OutputDisplay, string>;

const displayTitles = {
  ast: 'Abstract Syntax Tree',
  hir: 'High-level Intermediate Representation',
  jsast: 'JavaScript AST (ESTree)',
  javascript: 'JavaScript Code',
} satisfies Record<OutputDisplay, string>;

function defaultDisplay(): OutputDisplay {
  let params = getHashParams();
  let display: OutputDisplay = 'hir';

  if (params.has('display')) {
    display = params.get('display') as OutputDisplay;
  }

  if (params.get('runner') === 'compiler') {
    return 'javascript';
  }

  if (
    params.get('runner') === 'interpreter' &&
    (display === 'javascript' || display === 'jsast')
  ) {
    return 'hir';
  }

  return display;
}

function defaultExample() {
  let params = getHashParams();
  if (params.has('example')) {
    return params.get('example')!;
  }
  return Object.keys(examples)[0];
}

function defaultRunner() {
  let params = getHashParams();
  if (params.has('runner')) {
    return params.get('runner') as Runner;
  }
  return 'Interpreter';
}

function updateHashParam(key: string, value: string) {
  let params = getHashParams();
  params.set(key, value);
  window.location.hash = params.toString();
}

async function updateSourceHashparam(source: string) {
  let params = getHashParams();
  params.set('source', await compressSource(source));
  params.delete('example');
  window.location.hash = params.toString();
}

function updateExampleHashparam(example: string) {
  let params = getHashParams();
  params.set('example', example);
  params.delete('source');
  window.location.hash = params.toString();
}

function updateRunnerHashparam(runner: Runner) {
  updateHashParam('runner', runner);
}

async function updateDisplayHashparam(display: string) {
  updateHashParam('display', display);
}

// Optimization options are serialized as comma-separated `key:value` pairs in
// the `opt` hash param.  Only non-default values are stored to keep URLs clean.
// Short keys:  cf → constantFolding,  anf → anfTransform,  ro → anfReturnOpt,  dce → deadCodeElimination
function serializeOptimizations(options: JsOptimizationOptions): string {
  let parts: string[] = [];

  if (options.constantFolding === false) {
    parts.push('cf:false');
  }
  if (options.anfTransform === 'full') {
    parts.push('anf:full');
  }
  if (options.anfReturnOpt === false) {
    parts.push('ro:false');
  }
  if (options.deadCodeElimination === false) {
    parts.push('dce:false');
  }

  return parts.join(',');
}

function deserializeOptimizations(
  raw: string | null,
): JsOptimizationOptions | null {
  if (!raw) return null;

  const options: JsOptimizationOptions = {
    constantFolding: true,
    anfTransform: undefined,
    anfReturnOpt: true,
    deadCodeElimination: true,
  };

  for (const part of raw.split(',')) {
    const [key, value] = part.split(':');

    if (key === 'cf') {
      options.constantFolding = value !== 'false';
    } else if (key === 'anf') {
      options.anfTransform = value;
    } else if (key === 'ro') {
      options.anfReturnOpt = value !== 'false';
    } else if (key === 'dce') {
      options.deadCodeElimination = value !== 'false';
    }
  }

  return options;
}

function defaultOptimizationOptions(): JsOptimizationOptions {
  const params = getHashParams();
  return (
    deserializeOptimizations(params.get('opt')) ?? {
      constantFolding: true,
      anfTransform: undefined,
      anfReturnOpt: true,
      deadCodeElimination: true,
    }
  );
}

function updateOptimizationsHashParam(options: JsOptimizationOptions) {
  const params = getHashParams();
  const serialized = serializeOptimizations(options);

  if (serialized) {
    params.set('opt', serialized);
  } else {
    params.delete('opt');
  }

  window.location.hash = params.toString();
}

@customElement('tlang-playground')
export class TlangPlayground extends LitElement {
  static override styles = css`
    :host {
      display: contents;
    }

    t-shortcuts {
      display: flex;
      flex-direction: column;
      width: 100%;
      height: 100dvh;
    }

    main {
      overflow: hidden;
      flex: 1;
    }

    .repo-link {
      margin-right: 1ch;
      white-space: nowrap;
    }

    .repo-link a {
      text-decoration: none;
      color: var(--ctp-macchiato-mauve);
    }

    .repo-link a:hover {
      text-decoration: underline;
    }

    .anf-mode {
      display: flex;
      align-items: center;
    }

    .anf-mode > label {
      margin-right: 0.25em;
    }

    .editor-split {
      overflow: hidden;
    }

    .editor-split::part(first),
    .output-split::part(first) {
      display: flex;
    }

    .editor-panel {
      display: flex;
      flex-direction: column;
      overflow: hidden;
      flex: 1;
    }

    t-codemirror,
    .output-tabs {
      flex: 1;
    }

    .output-tabs {
      max-width: 100%;
      max-height: 100%;
    }

    .output-tabs > t-tab-panel {
      flex: 1;
      display: flex;
      max-width: 100%;
      overflow: hidden;
    }

    ::part(output),
    .output-code,
    .output-ast {
      width: 100%;
      overflow: auto;
    }

    pre {
      font-family: var(--t-font-family-mono, inherit);
      margin: 0;
    }

    .output-split::part(second) {
      flex: 0 0 min-content;
      max-height: calc(50% - 4px);
    }
  `;

  @mediaQuery('(min-width: 980px)')
  private desktop!: boolean;

  @state()
  private selectedExample = defaultExample();

  @state()
  source = examples[this.selectedExample];

  @state()
  private consoleMessages: ConsoleMessage[] = [];

  @state()
  private diagnosticMessages: DiagnosticMessage[] = [];

  @state()
  private diagnosticErrorCount = 0;

  @state()
  private diagnosticWarningCount = 0;

  @state()
  private display: OutputDisplay = defaultDisplay();

  private get availableDisplayOptions(): OutputDisplay[] {
    if (this.runner === 'Interpreter') {
      return ['ast', 'hir'];
    } else {
      return ['ast', 'hir', 'jsast', 'javascript'];
    }
  }

  @state() runner: Runner = defaultRunner();

  @state() optimizationOptions: JsOptimizationOptions =
    defaultOptimizationOptions();

  private tlang = new TlangController(
    this.source,
    this.runner,
    this.optimizationOptions,
  );

  // The editor which the user can use to write code, as it's always rendered,
  // we cache the query selector.
  @query('.editor', true)
  private codemirror!: TCodeMirror;

  @query('.output-split', true)
  private outputSplit!: SplitElement;

  @query('t-console', true)
  private consoleElement!: ConsoleElement;

  @state()
  hasLigatures = localStorage.getItem('ligatures') !== 'false';

  private toggleLigatures() {
    this.hasLigatures = !this.hasLigatures;
    localStorage.setItem('ligatures', String(this.hasLigatures));
  }

  private readonly hoverProvider = (pos: number) =>
    this.tlang.getHoverInfo(pos);

  private readonly gotoDefinitionProvider = (pos: number) =>
    this.tlang.getDefinitionLocation(pos);

  private async run() {
    if (!this.consoleElement.persist) {
      this.tlang.clearConsole();
    }
    await this.tlang.run(this.runner);
    this.consoleMessages = [...this.tlang.consoleMessages];
  }

  private async share() {
    await updateSourceHashparam(this.source);

    try {
      await navigator.clipboard.writeText(String(window.location));

      let flashnotification = new FlashElement();

      flashnotification.autoDismiss = 1000;
      flashnotification.textContent = 'URL copied to clipboard';
    } catch {
      let flashnotification = new FlashElement({ severity: 'error' });

      flashnotification.autoDismiss = 2000;
      flashnotification.textContent = 'Failed to copy URL to clipboard';
    }
  }

  protected override update(changedProperties: PropertyValueMap<this>): void {
    super.update(changedProperties);

    if (changedProperties.has('source') || changedProperties.has('runner')) {
      this.tlang.updateTlang(this.source, this.runner);
      const codemirrorDiagnostics = this.tlang.analyze();
      this.codemirror.diagnostics = codemirrorDiagnostics;
      this.codemirror.completionItems = this.tlang.getCompletionItems();
      this.diagnosticMessages = [...this.tlang.diagnosticMessages];
      this.diagnosticErrorCount = codemirrorDiagnostics.filter(
        (d) => d.severity === 'error',
      ).length;
      this.diagnosticWarningCount = codemirrorDiagnostics.filter(
        (d) => d.severity === 'warning',
      ).length;
      this.consoleMessages = [...this.tlang.consoleMessages];
    }

    if (changedProperties.has('hasLigatures')) {
      const on = +this.hasLigatures;
      this.style.setProperty(
        'font-feature-settings',
        `"calt" ${on}, "liga" ${on}`,
      );
    }
  }

  protected override firstUpdated(
    _changedProperties: PropertyValueMap<this>,
  ): void {
    defaultSource().then((source) => {
      this.codemirror.source = source;
    });
  }

  private handleSourceChange(event: CustomEvent) {
    this.source = event.detail.source;
  }

  private handleExampleSelect(event: Event) {
    const target = event.target as SelectElement;
    this.codemirror.source = examples[target.value];
    this.selectedExample = target.value;
    updateExampleHashparam(this.selectedExample);
  }

  private handleRunnerChange(event: Event) {
    const target = event.target as SelectElement;
    this.runner = target.value as Runner;

    // When using the interpreter, showing the javascript output does not make
    // sense.
    if (
      this.runner === 'Interpreter' &&
      (this.display === 'javascript' || this.display === 'jsast')
    ) {
      this.display = 'hir';
    }

    updateRunnerHashparam(this.runner);
  }

  private handleDisplayChange(event: CustomEvent<{ id: string }>) {
    this.display = event.detail.id as OutputDisplay;
    updateDisplayHashparam(this.display);
  }

  private toggleOptimization(key: keyof JsOptimizationOptions) {
    this.optimizationOptions = {
      ...this.optimizationOptions,
      [key]: !this.optimizationOptions[key],
    };
    this.tlang.setOptimizations(this.optimizationOptions);
    updateOptimizationsHashParam(this.optimizationOptions);
  }

  private setOptimizationOption<K extends keyof JsOptimizationOptions>(
    key: K,
    value: JsOptimizationOptions[K],
  ) {
    this.optimizationOptions = {
      ...this.optimizationOptions,
      [key]: value,
    };
    this.tlang.setOptimizations(this.optimizationOptions);
    updateOptimizationsHashParam(this.optimizationOptions);
  }

  private handleConsoleCollapse(event: CustomEvent<{ collapsed: boolean }>) {
    this.outputSplit.disabled = event.detail.collapsed ? 'resize-only' : false;

    if (event.detail.collapsed) {
      this.outputSplit.reset();
    } else {
      this.outputSplit.restore();
    }
  }

  private handleOutputSplitToggle(event: SplitEvent) {
    if (this.outputSplit.isTouched) {
      // Default behavior, reset the split.
      return;
    }

    event.preventDefault();

    this.outputSplit.restoreOrReset();
    this.consoleElement.collapse();
  }

  private handleConsoleClear(event: Event) {
    event.preventDefault();

    this.tlang.clearConsole();
    this.consoleMessages = [];
  }

  private showKeyboardShortcuts() {
    this.shadowRoot!.querySelector('t-shortcuts')!.showShortcutsReference();
  }

  private renderOutput() {
    switch (this.display) {
      case 'ast':
        return html`<pre class="output-ast">${this.tlang.getASTString()}</pre>`;
      case 'hir':
        return html`<t-hir-pretty
          .rawSource=${this.tlang.getHIRString()}
          .formatter=${(options: JsHirPrettyOptions) =>
            this.tlang.getHIRPretty(options)}
        ></t-hir-pretty>`;
      case 'jsast':
        return html`
          <t-codemirror
            class="output-code"
            language="json"
            .source=${this.tlang.getJSASTString()}
            readonly
          ></t-codemirror>
        `;
      case 'javascript':
        return html`
          <t-codemirror
            class="output-code"
            language="javascript"
            .source=${this.tlang.getJavaScript()}
            readonly
          ></t-codemirror>
        `;
    }
  }

  private hasOptimizationsEnabled(): boolean {
    return (
      this.optimizationOptions.constantFolding ||
      this.optimizationOptions.deadCodeElimination !== false ||
      (this.optimizationOptions.anfTransform != null &&
        this.optimizationOptions.anfTransform !== 'off')
    );
  }

  private renderExamplesSelect() {
    const exampleNames = Object.keys(examples);

    const ungroupedExamples = exampleNames.filter(
      (name) => !name.includes('/'),
    );
    const groupedExamples = exampleNames.filter((name) => name.includes('/'));
    const optGroups = Map.groupBy(
      groupedExamples,
      (name) => name.split('/')[0],
    );

    return html`
      <t-select
        class="toolbar__example"
        @change=${this.handleExampleSelect}
        .value=${live(this.selectedExample)}
      >
        ${repeat(
          ungroupedExamples,
          (key) => key,
          (key) => html`<option>${key}</option>`,
        )}
        ${repeat(
          optGroups.entries(),
          ([group, _]) => group,
          ([group, examples]) => {
            const label = group
              .split('_')
              .map((w) => `${w[0].toLocaleUpperCase()}${w.slice(1)}`)
              .join(' ');
            return html`<optgroup label=${label}>
              <legend>${label}</legend>
              ${repeat(
                examples,
                (key) => key,
                (key) =>
                  html`<option value=${key}>${key.split('/')[1]}</option>`,
              )}
            </optgroup>`;
          },
        )}
      </t-select>
    `;
  }

  protected override render() {
    return html`
      <t-shortcuts>
        <header>
          <t-toolbar>
            <t-button
              @click=${this.run}
              shortcut="ctrl+enter"
              shortcut-description="Run Code"
            >
              Run
            </t-button>
            <t-select
              class="toolbar__runner"
              @change=${this.handleRunnerChange}
              .value=${live(this.runner)}
            >
              <option value="Interpreter">Interpreter</option>
              <option value="JavaScript">Compiler (JS)</option>
            </t-select>
            <t-button
              popovertarget="optimization-options"
              aria-label="Optimization Settings"
            >
              <t-icon
                name=${this.hasOptimizationsEnabled()
                  ? 'lightning-on'
                  : 'lightning-off'}
              ></t-icon>
            </t-button>
            <t-menu id="optimization-options" popover=${floating()}>
              <t-menuitem-checkbox
                @change=${() => this.toggleOptimization('constantFolding')}
                .checked=${this.optimizationOptions.constantFolding}
              >
                Constant folding
              </t-menuitem-checkbox>
              <t-menuitem-checkbox
                @change=${() => this.toggleOptimization('deadCodeElimination')}
                .checked=${this.optimizationOptions.deadCodeElimination !==
                false}
              >
                Dead code elimination
              </t-menuitem-checkbox>
              ${this.runner === 'Interpreter'
                ? html`<t-menuitem-checkbox
                    @change=${() =>
                      this.setOptimizationOption(
                        'anfTransform',
                        this.optimizationOptions.anfTransform === 'full'
                          ? undefined
                          : 'full',
                      )}
                    .checked=${this.optimizationOptions.anfTransform === 'full'}
                  >
                    ANF transform
                  </t-menuitem-checkbox>`
                : html`<t-menuitem-group class="anf-mode">
                      <label id="anf-mode__label">ANF</label>
                      <t-menuitem-radio
                        aria-labelledby="anf-mode__label"
                        @click=${() =>
                          this.setOptimizationOption('anfTransform', 'minimal')}
                        .checked=${this.optimizationOptions.anfTransform !==
                        'full'}
                      >
                        Minimal
                      </t-menuitem-radio>
                      <t-menuitem-radio
                        aria-labelledby="anf-mode__label"
                        @click=${() =>
                          this.setOptimizationOption('anfTransform', 'full')}
                        .checked=${this.optimizationOptions.anfTransform ===
                        'full'}
                      >
                        Full
                      </t-menuitem-radio>
                    </t-menuitem-group>
                    <t-menuitem-checkbox
                      @change=${() => this.toggleOptimization('anfReturnOpt')}
                      .checked=${this.optimizationOptions.anfReturnOpt !==
                      false}
                    >
                      Return position optimization
                    </t-menuitem-checkbox>`}
            </t-menu>
            ${this.renderExamplesSelect()}
            <t-button @click=${this.share}>Share</t-button>
            <t-button
              slot="end"
              popovertarget="global-settings"
              aria-label="Global Settings"
            >
              <t-icon name="settings"></t-icon>
            </t-button>
            <t-menu id="global-settings" popover=${floating()}>
              <t-menuitem-checkbox
                @change=${this.toggleLigatures}
                ?checked=${this.hasLigatures}
              >
                Ligatures
              </t-menuitem-checkbox>
            </t-menu>
            <t-button
              slot="end"
              @click=${this.showKeyboardShortcuts}
              aria-label="Help"
            >
              ?
            </t-button>
            <div slot="end" class="repo-link">
              <a href="https://github.com/topaxi/tlang">Source Code</a>
            </div>
          </t-toolbar>
        </header>
        <main>
          <t-split
            class="editor-split"
            direction=${this.desktop ? 'vertical' : 'horizontal'}
          >
            <div slot="first" class="editor-panel">
              <t-codemirror
                class="editor"
                with-diagnostics
                .hoverProvider=${this.hoverProvider}
                .gotoDefinitionProvider=${this.gotoDefinitionProvider}
                @source-change=${this.handleSourceChange}
              ></t-codemirror>
              <t-diagnostics
                .messages=${this.diagnosticMessages}
                .errorCount=${this.diagnosticErrorCount}
                .warningCount=${this.diagnosticWarningCount}
              ></t-diagnostics>
            </div>
            <t-split
              slot="second"
              direction="horizontal"
              class="output-split"
              @t-split-toggle=${this.handleOutputSplitToggle}
            >
              <t-tabs
                class="output-tabs"
                slot="first"
                single
                .selected=${this.display}
                @t-tab-select=${this.handleDisplayChange}
              >
                ${repeat(
                  this.availableDisplayOptions,
                  (display) => display,
                  (display) => html`
                    <t-tab
                      slot="tab"
                      id=${display}
                      title=${displayTitles[display]}
                    >
                      ${displayLabels[display]}
                    </t-tab>
                  `,
                )}
                <t-tab-panel>
                  ${keyed(this.display, this.renderOutput())}
                </t-tab-panel>
              </t-tabs>
              <t-console
                slot="second"
                .messages=${this.consoleMessages}
                @collapse=${this.handleConsoleCollapse}
                @clear=${this.handleConsoleClear}
              >
              </t-console>
            </t-split>
          </t-split>
        </main>
      </t-shortcuts>
    `;
  }
}

declare global {
  interface HTMLElementTagNameMap {
    'tlang-playground': TlangPlayground;
  }
}
