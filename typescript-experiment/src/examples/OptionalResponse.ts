import { tangibleClient } from "./common.ts";

const tc = tangibleClient();

const response = await tc.expectPlainTextOption("What is the meaning of life for an intelligent alien?");
if (response.outcome === "Success") {
    console.log(response.value);
}