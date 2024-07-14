import { tangibleClient } from "./common.ts";


const tc = tangibleClient();

const response = await tc.expectPlainText("How are you?");
if (response.outcome === "Success") {
    console.log(response.value);
}