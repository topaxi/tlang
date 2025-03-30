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
  `;

  @property()
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
    this.hirPrettyOptions = {
      ...this.hirPrettyOptions,
      [option]: !this.hirPrettyOptions[option],
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
