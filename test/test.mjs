import { move } from "../dist/main.mjs";
import { assertEquals } from "https://deno.land/std@0.119.0/testing/asserts.ts";
import { ref } from "../dist/computed-properties.mjs";
import { createWindow, sleep } from "./util.mjs";

Deno.test("mustache parser", () => {
  const { proxy, document } = createWindow();
  let node;
  node = document.body.querySelector("div[data-container]");
  assertEquals(
    node.toString(),
    `<div data-container="123"><span><i>text</i></span><b><i>text</i></b>` +
      `<input value="foo"><ul></ul><div></div><div zh="中文"></div>` +
      `<div foo="bar"></div>raw text<span>str</span>` +
      `<div id="cmp">2</div><div id="nest"></div></div>`,
  );
  proxy.nested = { text: "hi" };
  node = document.body.querySelector("#nested");
  assertEquals(node.textContent, "hi");
});

Deno.test("lists", () => {
  const { proxy, document } = createWindow();
  const node = document.body.querySelector("ul");
  const moveStack = [];
  function mover() {
    moveStack.push(this["☯"]);
  }

  // init
  proxy.list = [
    { "☯": "a", [move]: mover },
    { "☯": "b", [move]: mover },
    { "☯": "c", [move]: mover },
  ];
  assertEquals(node.textContent, "abc");
  assertEquals(moveStack, []);

  // array mutation
  proxy.list.reverse();
  assertEquals(node.textContent, "cba");
  assertEquals(moveStack, ["c", "a"]);
  moveStack.length = 0;

  // array push
  proxy.list.push({ "☯": "d", [move]: mover });
  assertEquals(node.textContent, "cbad");
  assertEquals(moveStack, []);

  // array unshift
  proxy.list.unshift({ "☯": "z", [move]: mover });
  assertEquals(node.textContent, "zcbad");
  assertEquals(moveStack, ["d", "a", "b", "c"]);
  moveStack.length = 0;

  // array splice
  proxy.list.splice(1, 1, { "☯": "x", [move]: mover }, {
    "☯": "y",
    [move]: mover,
  });
  assertEquals(node.textContent, "zxybad");
  assertEquals(moveStack, ["d", "a", "b"]);
  moveStack.length = 0;

  // array sort
  proxy.list.sort(({ "☯": a }, { "☯": b }) => b.localeCompare(a));
  assertEquals(node.textContent, "zyxdba");
  assertEquals(moveStack, ["y", "x", "d", "b", "b", "a"]);
  moveStack.length = 0;

  // setting length should remove things
  proxy.list.length = 2;
  assertEquals(node.textContent, "zy");
  assertEquals(moveStack, []);

  // deleting entire array
  delete proxy.list;
  assertEquals(node.textContent, "");
  assertEquals(moveStack, []);

  // restoring array
  proxy.list = [{ "☯": "e", [move]: mover }, { "☯": "f", [move]: mover }];
  assertEquals(node.textContent, "ef");
  assertEquals(moveStack, []);
});

Deno.test("event binding", () => {
  const { document, proxy } = createWindow();
  let node;
  node = document.body.querySelector("b");
  node.click();
  assertEquals(node.textContent, "clicked!");
  proxy.input = "bar";
  node = document.body.querySelector("input");
  assertEquals(
    node.toString(),
    `<input value="bar">`,
  );
});

Deno.test("computed property", () => {
  const { source, document } = createWindow();
  const node = document.body.querySelector("#cmp");

  // simple assignment
  source.x.y.z = 2;
  assertEquals(node.textContent, "3");

  // deep assignment should not replace objects
  const oldY = source.x.y;
  source.x = { y: { z: 5 } };
  assertEquals(oldY, source.x.y);
  assertEquals(node.textContent, "6");

  // deletion should cause update
  delete source.x;
  assertEquals(node.textContent, "invalid");

  // re-assignment to ref should not update
  source.x = ref({ y: { z: 4 } });
  source.x.y.z = 2;
  assertEquals(node.textContent, "5");

  // overwriting with another object should restore deep observability
  source.x = { y: { z: 3 } };
  assertEquals(node.textContent, "4");
  source.x.y.z = 2;
  assertEquals(node.textContent, "3");
});

Deno.test("nested computed", async () => {
  const { source, document } = createWindow();
  const node = document.body.querySelector("#nest");

  // nested computeds
  assertEquals(node.textContent, "");
  source.foos = ["a", "b", "c"];
  assertEquals(node.textContent, "abc");
  source.foos[2] = { foo: "d" };
  assertEquals(node.textContent, "abd");
  source.foos[2].foo = "e";
  assertEquals(node.textContent, "abe");
  delete source.foos[2];
  assertEquals(node.textContent, "ab");
  source.foos = [];
  assertEquals(node.textContent, "");
  source.foos = ["x", "y", "z"];
  assertEquals(node.textContent, "xyz");
  delete source.foos;
  assertEquals(node.textContent, "");
  source.foos = ["1", "2", "3"];
  assertEquals(node.textContent, "123");
});
