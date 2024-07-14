import { tangibleClient } from "./common.ts";

const tc = tangibleClient();

const response = await tc.expectGroups(
    ["poodle", "american shorthair", "azawakh", "burmese"],
    new Set(["dog types", "cat types"])
);

if (response.outcome === "Success") {
    console.log(response.value);
}