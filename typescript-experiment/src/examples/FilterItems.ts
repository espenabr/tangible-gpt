import { tangibleClient } from "./common.ts";

const tc = tangibleClient();

const response = await tc.expectFiltered(
    [ "woof", "coffee", "meow", "baa", "oink", "grr", "pencil", "purr" ],
    "not animal sounds"
);

if (response.outcome === "Success") {
    console.log(response.value);
}