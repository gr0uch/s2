import bind, { root, target, mount, unmount, move, registerTemplate } from "./dist/main.mjs";
import parseMustache, { createMustacheTag } from "./dist/mustache.mjs";
import { observable, createComputed, ref } from "./dist/computed-properties.mjs";

const html = createMustacheTag(registerTemplate);
const computed = createComputed(mount, unmount);

export default bind;
export {
  // main
  bind, root, target, mount, unmount, move, registerTemplate,

  // mustache
  html, parseMustache,

  // computed
  observable, computed, ref,
};
