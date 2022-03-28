import s2 from "../../../api.mjs"; // s2-engine
import { viewModel, template } from "./example-view.mjs";

const [, fragment] = s2(viewModel, template);

// The following is a simple, fast way to ensure the container is empty.
// It is necessary for HMR to work.
document.body.innerHTML = "";

document.body.appendChild(fragment);
