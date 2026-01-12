import { LitElement, PropertyValueMap, css, html } from 'lit';
import { customElement, query, state } from 'lit/decorators.js';
import { examples } from './examples';
import { TlangController } from './controllers/tlang-controller';

import './components/t-button';
import './components/t-codemirror';
import './components/t-console';
import './components/t-hir-pretty';
import './components/t-live';
import './components/t-shortcuts';
import './components/t-split';
import './components/t-tabs';
import { type TCodeMirror } from './components/t-codemirror';
import { ConsoleElement, ConsoleMessage } from './components/t-console';
import { FlashElement } from './components/t-flash';
import { SplitElement, SplitEvent } from './components/t-split';
import { compressSource, decompressSource } from './utils/lz';
import { type Runner, type JsHirPrettyOptions } from './tlang';
import { keyed } from 'lit/directives/keyed.js';
import { live } from 'lit/directives/live.js';
import { repeat } from 'lit/directives/repeat.js';
import { mediaQuery } from './decorators/media-query';

type OutputDisplay = 'ast' | 'hir' | 'hir' | 'javascript';

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
  javascript: 'JavaScript',
} satisfies Record<OutputDisplay, string>;

const displayTitles = {
  ast: 'Abstract Syntax Tree',
  hir: 'High-level Intermediate Representation',
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

  if (params.get('runner') === 'interpreter' && display === 'javascript') {
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

    .toolbar {
      display: flex;
      gap: 1ch;
    }

    .repo-link {
      margin-left: auto;
      margin-right: 1rem;
    }

    .repo-link a {
      text-decoration: none;
      color: var(--ctp-macchiato-mauve);
    }

    .repo-link a:hover {
      text-decoration: underline;
    }

    .editor-split {
      overflow: hidden;
    }

    .editor-split::part(first),
    .output-split::part(first) {
      display: flex;
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
      font-family: inherit;
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
  private display: OutputDisplay = defaultDisplay();

  private get availableDisplayOptions(): OutputDisplay[] {
    if (this.runner === 'Interpreter') {
      return ['hir', 'ast'];
    } else {
      return ['javascript', 'hir', 'ast'];
    }
  }

  @state() runner: Runner = defaultRunner();

  private tlang = new TlangController(this.source, this.runner);

  // The editor which the user can use to write code, as it's always rendered,
  // we cache the query selector.
  @query('.editor', true)
  private codemirror!: TCodeMirror;

  @query('.output-split', true)
  private outputSplit!: SplitElement;

  @query('t-console', true)
  private consoleElement!: ConsoleElement;

  private run() {
    if (!this.consoleElement.persist) {
      this.tlang.clearConsole();
    }
    this.tlang.run(this.runner);
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
      this.codemirror.diagnostics = this.tlang.analyze();
      this.consoleMessages = [...this.tlang.consoleMessages];
    }
  }

  protected override firstUpdated(
    _changedProperties: PropertyValueMap<this>,
  ): void {
    let params = new URLSearchParams(window.location.hash.slice(1));

    let exampleName = String(params.get('example'));

    if (exampleName in examples) {
      this.shadowRoot!.querySelector('select')!.value = exampleName;
    }

    defaultSource().then((source) => {
      this.codemirror.source = source;
    });
  }

  protected override updated(changedProperties: PropertyValueMap<this>): void {
    if (changedProperties.has('runner')) {
      // TODO: This is a hack to update the select value. Lit doesn't seem to
      //       be super happy about dynamically updated options.
      let select =
        this.shadowRoot?.querySelector<HTMLSelectElement>('.toolbar__display');

      if (select) {
        select.value = this.display;
        updateDisplayHashparam(this.display);
      }
    }
  }

  private handleSourceChange(event: CustomEvent) {
    this.source = event.detail.source;
  }

  private handleExampleSelect(event: Event) {
    const target = event.target as HTMLSelectElement;
    this.codemirror.source = examples[target.value];
    this.selectedExample = target.value;
    updateExampleHashparam(this.selectedExample);
  }

  private handleRunnerChange(event: Event) {
    const target = event.target as HTMLSelectElement;
    this.runner = target.value as Runner;

    // When using the interpreter, showing the javascript output does not make
    // sense.
    if (this.runner === 'Interpreter' && this.display === 'javascript') {
      this.display = 'hir';
    }

    updateRunnerHashparam(this.runner);
  }

  private handleDisplayChange(event: CustomEvent<{ id: string }>) {
    this.display = event.detail.id as OutputDisplay;
    updateDisplayHashparam(this.display);
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

  protected override render() {
    return html`
      <t-shortcuts>
        <header>
          <div class="toolbar">
            <t-button
              @click=${this.run}
              shortcut="ctrl+enter"
              shortcut-description="Run Code"
            >
              Run
            </t-button>
            <select
              class="toolbar__runner"
              @change=${this.handleRunnerChange}
              .value=${live(this.runner)}
            >
              <option value="Interpreter">Interpreter</option>
              <option value="JavaScript">Compiler (JS)</option>
            </select>
            <t-button @click=${this.share}>Share</t-button>
            <select
              class="toolbar__example"
              @change=${this.handleExampleSelect}
              .value=${live(this.selectedExample)}
            >
              ${repeat(
                Object.keys(examples),
                (key) => key,
                (key) => html`<option>${key}</option>`,
              )}
            </select>
            <t-button @click=${this.showKeyboardShortcuts} aria-label="Help">
              ?
            </t-button>
            <div class="repo-link">
              <a href="https://github.com/topaxi/tlang">Source Code</a>
            </div>
          </div>
        </header>
        <main>
          <t-split
            class="editor-split"
            direction=${this.desktop ? 'vertical' : 'horizontal'}
          >
            <t-codemirror
              slot="first"
              class="editor"
              with-diagnostics
              @source-change=${this.handleSourceChange}
            ></t-codemirror>
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
