import { observable } from "../../../api.mjs"; // s2-engine

const initialState = {
  name: "Goober",
};

// Restore observable state from HMR.
const state = observable(
  globalThis.__state__ ?
    JSON.parse(JSON.stringify(globalThis.__state__)) :
    initialState);

// Preserve observable state across HMR.
globalThis.__state__ = state;

export default state;
