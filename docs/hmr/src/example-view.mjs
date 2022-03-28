import { computed, html, unmount } from "../../../api.mjs"; // s2-engine
import state from "./example-state.mjs";
import { css } from "goober";

export const viewModel = computed({
  inputName() {
    return state.name;
  },
  updateName(event) {
    state.name = event.target.value;
  },
  containerClass() {
    return css`
      color: purple;
    `;
  },
  displayName() {
    return state.name;
  },
  [unmount]() {
    // Notice that nowhere in the src code do we manually call unmount,
    // this is called automatically when the root node is removed from
    // the DOM.
    console.log("called unmount");
  },
});

export const template = html`
  <div class="{{containerClass}}">
    <input type="text" value="{{inputName}}" oninput="{{updateName}}">
    <p>
      Hi {{displayName}}!
    </p>
    <p>
      Type something in the input, and then try to change the text color.
      Notice that the input is preserved, while the template has changed :)
    </p>
  </div>
`;
