// deno script
import s2, { mount, unmount, registerTemplate } from "../dist/main.mjs";
import parseMustache from "../dist/mustache.mjs";
import { observable, createComputed } from "../dist/computed-properties.mjs";
import { parseHTML } from "https://cdn.jsdelivr.net/gh/WebReflection/linkedom@latest/worker.js";

const computed = createComputed(mount, unmount);

const { window, document } = parseHTML(`
<!DOCTYPE html>
<html></html>
`);

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

// TODO: currently, linkedom doesn't play well with the mustache parser
// const template = parseMustache(`
// <div
//   foo="{{foo}}"
// >
//   wtf {{yo}}
//   {{#comp}}
//     wololo
//     {{y}}
//     {{x}}
//   {{/comp}}
// </div>
// `);

const template = document.createElement("div");
template.innerHTML = `
<div data-foo="foo">
  wtf <span data-text="yo"></span>
  <slot name="comp" data-template="comp"></slot>
</div>
`;

const comp = document.createElement("div");
comp.innerHTML = `
wololo
<span data-text="y"></span>
<span data-text="x"></span>
`;

registerTemplate("comp", comp);

const [proxy, fragment] = s2(obj, template);

document.body.appendChild(fragment);
console.log(document.toString());
