# s²

**s2 is a ~2kb metaprogramming function that maps data structures to the DOM.**

It returns a `Proxy` so that plain JS objects (data + functions), can map directly to elements. Data structures are bound to the DOM, using HTML `<template>` & `<slot>` tags, and `data-` attributes to bind data and events.

The result is that *UI code effectively disappears*, it is folded into the data.

>Even the simplest procedural logic is hard for humans to verify, but quite complex data structures are fairly easy to model and reason about... Data is more tractable than program logic. It follows that where you see a choice between complexity in data structures and complexity in code, choose the former. More: in evolving a design, you should actively seek ways to shift complexity from code to data.

— Eric S. Raymond, [Basics of the Unix Philosophy](http://www.catb.org/~esr/writings/taoup/html/ch01s06.html)


## Usage

Import the module:

```js
import s2 from "https://cdn.jsdelivr.net/gh/gr0uch/s2@latest/dist/main.min.js";
```

Trivial example of composing templates, binding text and events:

```html
<template id="root">
  <h1>Hello, world!</h1>
  <slot name="counter" data-template="#countbox">Empty</slot>
</template>
<template id="countbox">
  <span data-text="count"></span>
  <button data-event-click="increment">Increment</button>
</template>
```

Binding data:

```js
const template = document.getElementById("root");
const [proxy, node] = s2({
  counter: {
    count: 0,
    increment() {
      this.count++;
    },
  }
}, template);
document.body.appendChild(node);
```

Finally: *any changes on the entire proxy object will reflect in the DOM!* **This includes deeply nested structures as well.**

```js
proxy.counter.count = 1337; // DOM updates
proxy.counter.increment = function() { this.count-- } // Overwrite event listener
delete proxy.counter; // Elements removed
```

Here are the data attributes it will look for:

- `data-text`, `data-unsafe-html`
- `data-class`
- `data-value`
- `data-event-*`
- `data-attribute-*`


## Mount and Unmount

Each object may implement a `mount` and `unmount` function. This allows you to do animations and run any code needed on these events. The unmount function is particularly useful for implementing exiting animations, as it will wait for a promise to resolve before removing the node.

```js
// These named exports are symbols that prevent key collisions.
import { mount, unmount } from '...';

{
  [mount]: function(node) {
    // `this` will be called in the proxy context.
  },
  [unmount]: async function(node) {
    // Animations can be implemented here while waiting to remove the node.
    await new Promise(resolve => setTimeout(resolve, 1000));
  },
}
```


## Optional Modules

- `dep-check.js`: this is used to check if all of the required APIs are present in the runtime.
- `render-to-string.js` (TBD)
- `hydrate.js` (TBD)


## Benchmarks

See [js-framework-benchmark results table](https://krausest.github.io/js-framework-benchmark/current.html).


## Web Components

s2 is agnostic about most of [Web Components](https://developer.mozilla.org/en-US/docs/Web/Web_Components), since the specification doesn't play well with server-side rendered HTML without some non-trivial work. It aims to focus on orthogonal functionality like data binding.

Although s2 uses `<template>` and `<slot>` elements, it does so in a different context. For example, it is not possible to use the `slot` attribute to fill content in a slot, since s2 binds the slot to data rather than elements.


## Name

s2 can mean:
- simulacra 2: electric bugaloo
- super solenoid
- sneaky submarine
- season 2
- 2 kilobytes compressed


## Development

s2 is written in the Parenscript subset of Common Lisp.

- [`parenscript-builder`](https://github.com/gr0uch/parenscript-builder): see instructions
- [`terser`](https://github.com/terser/terser): `npm i -g terser` to add this executable to `$PATH`

Need to build the `psbuild` binary from `parenscript-builder` and put it here to compile with `make`. I couldn't figure out how to automate including this dependency yet.


## Testing

Run a web server like `http-server .` and then navigate to the `/test/` directory. HTTP is required for loading modules.


## License

[BSD 3-Clause](https://github.com/gr0uch/s2/blob/master/LICENSE)
