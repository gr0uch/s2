import { parseHTML } from "https://cdn.jsdelivr.net/gh/WebReflection/linkedom@latest/worker.js";
import s2, { mount, registerTemplate, unmount } from "../dist/main.mjs";
import parseMustache, { createMustacheTag } from "../dist/mustache.mjs";
import { createComputed, observable } from "../dist/computed-properties.mjs";

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
    }),
  };

  const nested = mustache`<div id="nested">{{text}}</div>`;
  const template = mustache`
  <div
    data-container="{{id}}"
  >
    {{!comment-should-be-stripped}}
    unescaped <b onclick="{{click}}">{{{unescaped}}}</b>
    <input value="{{input}}">
    <ul>
      {{#list}}
        <li>{{text}}</li>
      {{/list}}
    </ul>
    {{#nested}}
      ${nested}
    {{/nested}}
    {{!another-comment}}
    {{#comp}}
      raw text
      {{str}}
      <div id="cmp">{{value}}</div>
    {{/comp}}
  </div>
  `;

  cleanupTemplate(document, template);
  const [proxy, fragment] = s2(obj, template);
  document.body.appendChild(fragment);

  return { source, proxy, window, document };
}
