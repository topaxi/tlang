import { css, html, LitElement, TemplateResult } from 'lit';
import { floating } from '../directives/floating';
import { customElement, property, state } from 'lit/decorators.js';
import { type Tlang } from 'tlang_bindings_js';

import './t-button';
import './t-menu';
import './t-codemirror';

@customElement('t-hir-pretty')
export class HirPrettyElement extends LitElement {
  static override styles = css`
    :host {
      display: contents;
    }

    t-button {
      position: absolute;
      top: 0;
      right: 0;
      z-index: 1;
    }

    pre {
      font-family: inherit;
      margin: 0;
    }

    .indent-size {
      display: flex;
      align-items: center;
    }

    .indent-size::before {
      content: '\\00a0\\00a0';
    }

    .indent-size > label {
      margin-right: 0.25em;
    }

    .indent-size:has([aria-disabled='true']) > label {
      opacity: 0.5;
    }
  `;

  @property({ attribute: false })
  tlang: Tlang | null = null;

  @state()
  private pretty = true;

  @state()
  private hirPrettyOptions = {
    indentSize: 4,
    tabIndent: false,
    markUnresolved: true,
    comments: false,
  };

  private toggleHirPrettyOption(option: keyof typeof this.hirPrettyOptions) {
    this.setHirPrettyOption(option, !this.hirPrettyOptions[option]);
  }

  private setHirPrettyOption<T extends keyof typeof this.hirPrettyOptions>(
    option: T,
    value: (typeof this.hirPrettyOptions)[T],
  ) {
    this.hirPrettyOptions = {
      ...this.hirPrettyOptions,
      [option]: value,
    };
  }

  protected override render(): TemplateResult {
    return html`
      <t-button
        popovertarget="hir_pretty-options"
        aria-label="HIR Format Settings"
      >
        
      </t-button>
      <t-menu id="hir_pretty-options" popover=${floating()}>
        <t-menuitem-checkbox
          .disabled=${!this.pretty}
          @change=${() => this.toggleHirPrettyOption('comments')}
          .checked=${this.hirPrettyOptions.comments}
        >
          Show comments
        </t-menuitem-checkbox>
        <t-menuitem-checkbox
          .disabled=${!this.pretty}
          @change=${() => this.toggleHirPrettyOption('markUnresolved')}
          .checked=${this.hirPrettyOptions.markUnresolved}
        >
          Mark unresolved
        </t-menuitem-checkbox>
        <t-menuitem-checkbox
          .disabled=${!this.pretty}
          @change=${() => this.toggleHirPrettyOption('tabIndent')}
          .checked=${this.hirPrettyOptions.tabIndent}
        >
          Tab indent
        </t-menuitem-checkbox>
        <t-menuitem-group class="indent-size">
          <label id="indent-size__label">Indent size</label>
          <t-menuitem-radio
            aria-labelledby="indent-size__label"
            .disabled=${this.hirPrettyOptions.tabIndent || !this.pretty}
            @click=${() => this.setHirPrettyOption('indentSize', 2)}
            .checked=${this.hirPrettyOptions.indentSize === 2}
          >
            2
          </t-menuitem-radio>
          <t-menuitem-radio
            aria-labelledby="indent-size__label"
            .disabled=${this.hirPrettyOptions.tabIndent || !this.pretty}
            @click=${() => this.setHirPrettyOption('indentSize', 4)}
            .checked=${this.hirPrettyOptions.indentSize === 4}
          >
            4
          </t-menuitem-radio>
          <t-menuitem-radio
            aria-labelledby="indent-size__label"
            .disabled=${this.hirPrettyOptions.tabIndent || !this.pretty}
            @click=${() => this.setHirPrettyOption('indentSize', 8)}
            .checked=${this.hirPrettyOptions.indentSize === 8}
          >
            8
          </t-menuitem-radio>
        </t-menuitem-group>
        <t-menuitem-checkbox
          @change=${() => (this.pretty = !this.pretty)}
          .checked=${this.pretty}
        >
          Pretty print
        </t-menuitem-checkbox>
      </t-menu>
      ${this.pretty
        ? html`<t-codemirror
            part="output pretty"
            language="tlang"
            .source=${this.tlang?.getHIRPretty(this.hirPrettyOptions)}
            readonly
          ></t-codemirror>`
        : html`<pre part="output raw">${this.tlang?.getHIRString()}</pre>`}
    `;
  }
}
