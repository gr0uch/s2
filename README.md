# s2

**s2 is a metaprogramming function that enables web user interfaces to be mapped as data structures.**

It works by returning a `Proxy` so that plain JS objects (data + functions), can map directly to elements. It uses `Proxy` objects to bind data to the DOM, HTML `<template>` & `<slot>` tags, and `data-` attributes to bind data and events.

The result is that *UI code effectively disappears*, it is folded into the data.

>Even the simplest procedural logic is hard for humans to verify, but quite complex data structures are fairly easy to model and reason about... Data is more tractable than program logic. It follows that where you see a choice between complexity in data structures and complexity in code, choose the former. More: in evolving a design, you should actively seek ways to shift complexity from code to data.

— Eric Raymond, [Basics of the Unix Philosophy](http://www.catb.org/~esr/writings/taoup/html/ch01s06.html)


## TODO (INTERNAL)

- [x] Set array slots
- [x] Set array length should delete
- [x] Set slot empty state
- [x] Set class
- [x] Set attribute
- [ ] Mount/unmount functions (animations)
- [ ] Automated testing


## Usage

Import the module:

```js
import s2 from "https://cdn.jsdelivr.net/gh/gr0uch/s2@latest/dist/main.min.js";
```

Trivial example of composing templates, binding text and events:

```html
<template id="menu">
  <h1>Hello, world!</h1>
  <slot name="counter" data-template="#counter">Empty</slot>
</template>
<template id="counter">
  <span data=text="count"></span>
  <button data-event-click="increment">Increment</button>
</template>
```

Binding data:

```js
const template = document.getElementById("menu");
const [node, proxy] = s2({
  counter: {
    count: 0,
    increment() {
      this.count++;
    },
  }
}, template);
document.body.appendChild(node);
```

Finally: *any changes on the entire proxy object will reflect in the DOM!* This includes nested structures as well.

```js
proxy.counter.count = 1337; // DOM updates
delete proxy.counter; // Elements removed
```

Here are the data attributes it will look for:

- `data-text`, `data-unsafe-html`
- `data-event-*`
- `data-class`
- `data-attribute-*` (TBD)


## Caveats

s2 relies on Proxy objects throughout, so one can not use references to the original objects and expect it to work. Instead, always get new references by accessing keys via the Proxy.


## Web Components

s2 is agnostic about most of [Web Components](https://developer.mozilla.org/en-US/docs/Web/Web_Components), since the specification doesn't play well with server-side rendered HTML without some non-trivial work. It aims to focus on orthogonal functionality like data binding.

Although s2 uses `<template>` and `<slot>` elements, it does so in a different context. For example, it is not possible to use the `slot` attribute to fill content in a slot, since s2 binds the slot to data rather than elements.


## Name

s2 can mean:
- simulacra 2: electric bugaloo
- super solenoid
- sneaky submarine
- season 2


## Development

Requirements:

- [`parenscript-builder`](https://github.com/gr0uch/parenscript-builder): see instructions
- [`terser`](https://github.com/terser/terser): `npm i -g terser` to add this executable to `$PATH`

Need to build the `psbuild` binary from `parenscript-builder` and put it here to compile with `make`. I couldn't figure out how to automate including this dependency yet.


## License

[BSD 3-Clause](https://github.com/gr0uch/s2/blob/master/LICENSE)
