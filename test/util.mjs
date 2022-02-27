import { parseHTML } from "https://cdn.jsdelivr.net/gh/WebReflection/linkedom@latest/worker.js";
import s2, { mount, registerTemplate, unmount } from "../dist/main.mjs";
import parseMustache, { createMustacheTag } from "../dist/mustache.mjs";
import { createComputed, observable } from "../dist/computed-properties.mjs";

// Opt-in behavior.
parseMustache.selfClosing = true;

const computed = createComputed(mount, unmount);
const mustache = createMustacheTag(registerTemplate);

export function cleanupTemplate(document, template) {
  const iterator = document.createNodeIterator(template.content, 4);
  let currentNode;
  while ((currentNode = iterator.nextNode())) {
    currentNode.textContent = currentNode.textContent.trim();
    if (!currentNode.textContent) currentNode.remove();
  }
}

export function createWindow() {
  const { window } = parseHTML(`<!DOCTYPE html><html></html>`);
  const { document } = window;

  // Careful, this is stateful.
  s2.window = parseMustache.window = window;

  const source = observable({
    foos: null,
    x: {
      y: {
        z: 1,
      },
    },
  }, true);

  const obj = {
    id: 123,
    unescaped: "<i>text</i>",
    input: "foo",
    click() {
      this.unescaped = "clicked!";
    },
    comp: computed({
      str: "str",
      value() {
        return (source.x?.y?.z + 1) || "invalid";
      },
      nest() {
        const { foos } = source;
        if (!foos) return null;
        return foos.map((_, i) => {
          return computed({
            text() {
              const { foos } = source;
              const value = foos[i];
              if (typeof value !== "object") return value;
              return value.foo;
            }
          });
        });
      }
    }),
  };

  const nested = mustache`<div id="nested">{{text}}</div>`;
  const foo = mustache`<div class="foo">{{text}}</div>`;
  const template = mustache`
  <div
    data-container="{{id}}"
  >
    {{!comment-should-be-stripped}}
    {{{ unescaped }}}
    <b onclick="{{ click }}">{{{ unescaped }}}</b>
    <input value="{{ input }}">
    <ul>
      {{#list}}
        <li>{{ ☯ }}</li>
      {{/list}}
    </ul>
    <div />
    <div zh="中文" />
    <div
      foo="bar"
    />
    {{#
      nested
    }}
      ${nested}
    {{/
      nested
    }}
    {{!
      another-comment
    }}
    {{#comp}}
      raw text
      {{
        str
      }}
      <div id="cmp">
        {{
          value
        }}
      </div>
      <div id="nest">
        {{#nest}}
          ${foo}
        {{/nest}}
      </div>
    {{/comp}}
  </div>
  `;

  cleanupTemplate(document, template);
  const [proxy, fragment] = s2(obj, template);
  document.body.appendChild(fragment);

  return { source, proxy, window, document };
}

export function customWindow(obj, template) {
  const { window } = parseHTML(`<!DOCTYPE html><html></html>`);
  const { document } = window;
  s2.window = parseMustache.window = window;
  template = mustache`${template}`;
  cleanupTemplate(document, template);
  const [proxy, fragment] = s2(obj, template);
  document.body.appendChild(fragment);
  return { proxy, window, document };
}
