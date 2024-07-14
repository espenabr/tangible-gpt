import { tangibleClient } from "./common.ts";

const tc = tangibleClient();

const response = await tc.expectEnumCase(
    "What is the most common color of a swan?",
    ["red", "blue", "black", "white"],
);
if (response.outcome === "Success") {
    console.log(response.value);
}
