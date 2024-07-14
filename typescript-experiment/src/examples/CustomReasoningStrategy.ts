import { tangibleClient } from "./common.ts";

const tc = tangibleClient();

const response = await tc.expectNumber(
    "A juggler has 16 balls. Half of the balls are golf balls and half of the golf balls are blue. How many blue golf balls are there?",
    [],
    [],
    "ThinkStepByStep",
);

if (response.outcome === "Success") {
    console.log(response.value);
}