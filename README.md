# s2

Super simple data binding for the DOM.

```html
<template id="menu">
  <div data-key="options">
    <button
      data-text="label"
      data-class="cls"
      data-event-click="choose"
    ></button>
  </div>
</template>
```

```js
const template = document.getElementById('menu');
const choose = () => {};
const [binding, element] = s2({
  options: [
    { label: 'Red', choose },
    { label: 'Blue', choose },
  ],
}, template);
document.body.appendChild(element);
```

## Usage

Import the module:

```js
import s2 from "...";
```

## Development

Uses `parenscript-builder`, need to build the `psbuild` binary and put it here to compile.
