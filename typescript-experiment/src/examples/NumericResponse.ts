import { tangibleClient } from "./common.ts";

const tc = tangibleClient();

const response = await tc.expectNumber("Approximately how many people live in Norway?");
if (response.outcome === "Success") {
    console.log(response.value);
}
