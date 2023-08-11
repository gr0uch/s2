import { parseHTML } from "https://cdn.jsdelivr.net/gh/WebReflection/linkedom@latest/worker.js";
import Mustache from "https://esm.run/mustache";
import * as s2 from "../api.mjs";

const { window } = parseHTML(`<!DOCTYPE html><html></html>`);
const { document } = window;

s2.default.window = s2.parseMustache.window = window;
const bind = s2.default;

const data = {
  add, todos: [
    'Sleep', 'Wake up',
    'Brush teeth', 'Get dressed',
  ].map(todo => ({
    t: "line-through",
    todo, toggle, remove,
  })),
};

function add(event) {
  event.preventDefault();
  this[root].todos.unshift({
    todo: this.todo,
    remove, toggle, });
  delete this.todo;
}
function toggle() {
  this.t = !this.t ?
    "line-through" : null;
}
function remove() {
  const i = this[root].todos
    .findIndex(_ => _ === this);
  this[root].todos.splice(i, 1);
}

const rawTemplate = `
<form onsubmit="{{add}}">
  <input required
    value="{{todo}}"
    placeholder="Todo...">
  <button>Add</button>
</form>
<ul>
  {{#todos}}
    <li>
      <div onclick="{{remove}}">
        ✕</div>
      <label>
        <input
          type="checkbox"
          oninput="{{toggle}}">
        <span
          style:text-decoration="{{t}}"
        >
          {{todo}}
        </span>
      </label>
    </li>
  {{/todos}}
</ul>`;

const template = s2.html`${rawTemplate}`;

const [proxy, node] = bind(data, template);
document.body.append(node);
console.log(document.body.innerHTML);

function sanitize(d) {
  for (const key in d) {
    const v = d[key];
    if (v && typeof v === "object") {
      sanitize(v);
    }
    if (typeof v === "function") {
      delete d[key];
    }
  }
  return d;
}

sanitize(data);

console.log(Mustache.render(rawTemplate, data));
