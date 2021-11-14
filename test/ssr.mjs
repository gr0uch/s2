// deno script
import s2, { mount, unmount, registerTemplate } from "../dist/main.mjs";
import parseMustache from "../dist/mustache.mjs";
import { observable, createComputed } from "../dist/computed-properties.mjs";
import { parseHTML } from "https://cdn.jsdelivr.net/gh/WebReflection/linkedom@latest/worker.js";

const computed = createComputed(mount, unmount);

const { window } = parseHTML(`<!DOCTYPE html><html></html>`);
const { document } = window;

s2.window = parseMustache.window = window;
// s2.debug = true;

const obs = observable({
    x: 1,
});

const obj = {
    foo: "bar",
    yo: "bbq",
    comp: computed({
        y: "y",
        x() {
            return obs.x + 1;
        }
    }),
};

const template = parseMustache(`
<div
  foo="{{foo}}"
>
  {{!lol}}
  wtf <b>{{{yo}}}</b>
  {{#ayy}}
    {{>lmao}}
  {{/ayy}}
  {{!kek}}
  {{#comp}}
    wololo
    {{{y}}}
    {{x}}
  {{/comp}}
</div>
`);

const [proxy, fragment] = s2(obj, template);

document.body.appendChild(fragment);

console.log(document.toString());
