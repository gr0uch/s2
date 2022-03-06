import s2, { mount, move, registerTemplate, unmount } from "../dist/main.mjs";
import depCheck from "../dist/dep-check.mjs";
import parseMustache, { createMustacheTag } from "../dist/mustache.mjs";
import { createComputed, observable } from "../dist/computed-properties.mjs";
import test from "./runner.mjs";

// s2.debug = true;
// s2.isDeferred = true;
depCheck();
cleanupTemplates();

const start = performance.now();

function moveThing(node) {
  console.log("MOVE", this.text, node);
}

registerTemplate("reg", '<b data-text="a"></b>');
const mustacheStr = `
<div
  foo="{{foo}}"
  class="{{cls}}"
  data-something="{{something}}">
  {{! comment }}
  <span onclick="{{clicky}}">{{txt}}</span>
  <span>{{{html}}}</span>
  {{#ul}}
    <span>{{name}}</span>
    {{#deep}}
      <span>{{value}}</span>
    {{/deep}}
  {{/ul}}
  wtf {{yo}}
  {{#ol}}
    {{>reg}}
  {{/ol}}
  {{{danger}}}
</div>
`;
const mustacheTmpl = parseMustache(mustacheStr);
registerTemplate("must", mustacheTmpl);

import("https://unpkg.com/mustache@4.2.0/mustache.mjs").then((mod) => {
  const { default: Mustache } = mod;
  const data = {
    cls: "cls",
    clicky: function () {},
    txt: "txt",
    html: "<b>html</b>",
    ul: [
      {
        name: "name",
        deep: {
          value: "value",
        },
      },
    ],
    yo: "yo",
  };
  const output = Mustache.render(mustacheStr, data);
  const [_, fragment] = s2(data, "must");
  const div = document.createElement("div");
  div.appendChild(fragment);
  console.log("^^ mustache", output);
  console.log("^^ s2", div.innerHTML, mustacheTmpl);
});

(() => {
  const mustache = createMustacheTag(registerTemplate);
  const p1 = mustache`{{a}} {{b}}`;
  const p2 = mustache`tagged template {{#name}}${p1}{{/name}}`;
  const [_, fragment] = s2({ name: { a: "a", b: "b" } }, p2);
  const div = document.createElement("div");
  div.appendChild(fragment);
  console.log(div.innerHTML);
})();

const initialThings = [
  { text: "a", [move]: moveThing },
  { text: "b", [move]: moveThing },
  { text: "c", [move]: moveThing },
];

test.initialThings = initialThings;

const source = observable({
  showBox: true,
  array: new Array(3).fill(true),
  selectedIndex: 0,
  content: "f",
});
const computed = createComputed(mount, unmount);

const template = document.querySelector("#root");

const [reactiveObject, node] = s2({
  title: () => "Hello, <em>world!</em>",
  titleClass: "title",
  fooBar: "bar",
  titleStyle: "color: #ff8;",
  list: {
    descriptionColor: "#08f",
    descriptionBackground: "#000",
    things: initialThings.slice(),
    "try": [
      { run, code: `p.list.things.reverse()` },
      { run, code: `p.list.things.push({ text: 'd' })` },
      { run, code: `p.list.things.unshift({ text: 'z' })` },
      { run, code: `p.list.things.splice(1, 1, { text: 'x' }, { text: 'y' })` },
      {
        run,
        code:
          `p.list.things.sort(({ text: a }, { text: b }) => b.localeCompare(a))`,
      },
      { run, code: `p.list.things.length = 2` },
      { run, code: `delete p.list.things` },
      { run, code: `p.list.things = [ { text: 'e' }, { text: 'f' } ]` },
    ],
  },
  counter: {
    count: 0,
    total: 0,
    runText: "Run test",
    test,
    [mount]: function (node) {
      console.log("Mount called");
    },
    [unmount]: delayUnmount,
  },
  container: computed({
    box() {
      if (!source.showBox) return null;
      return source.array.map((_, i) => {
        if (!source.array[i]) return null;
        return computed({
          i,
          content() {
            return `${Math.random()} // ${source.content} ${i}`;
          },
          content2() {
            return source.selectedIndex === i ? "%" : null;
          },
          select() {
            source.selectedIndex = i;
          },
          remove(event) {
            const a = source.array.slice();
            delete a[i];
            source.array = a;
            console.log("wtf", source.array, this);
          },
        });
      });
    },
  }),
  reg: { a: 1 },
  useHref: "#foo-test",
  [mount]: function spam(node) {
    // return null;
    const t = {};
    let i, n, c;
    n = document.createElement("span");
    n.textContent = "_";
    for (i = 0; i < 10; i++) {
      c = n.cloneNode(true);
      node.appendChild(document.createComment("fak"));
      node.appendChild(c);
      node.appendChild(document.createComment("u"));
    }
  },
}, template);

document.body.appendChild(node);
console.log(`Mounted in ${performance.now() - start} ms`);

window.p = reactiveObject;
window.s = source;

async function delayUnmount(node) {
  console.log("Unmount called", node, this);
  node.style.color = "#f35";
  await new Promise((resolve) => setTimeout(resolve, 500));
}

function run() {
  eval(this.code);
}

function cleanupTemplates() {
  document.querySelectorAll("template").forEach((template) => {
    const iter = document
      .createNodeIterator(template.content, NodeFilter.SHOW_TEXT);
    let node;
    while (node = iter.nextNode()) {
      if (!node.nodeValue.trim().length) {
        node.remove();
      }
    }
  });
}
