import { tangibleClient } from "./common.ts";


const tc = tangibleClient();

const response = await tc.expectBoolean("Is AI smarter than most humans?");
if (response.outcome === "Success") {
    console.log(response.value);
}