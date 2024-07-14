import { Column } from "../model.ts";
import { tangibleClient } from "./common.ts";

const tc = tangibleClient();

const columns: Column[] = [
    { type: "TextColumn", name: "Name" },
    {
        type: "EnumColumn",
        name: "Animal",
        options: ["Duck", "Goose", "Mouse", "Dog", "Cow"],
    },
    { type: "TextColumn", name: "Occupation" },
    { type: "TextColumn", name: "Personality" },
    { type: "BooleanColumn", name: "Is old" },
];

const response = await tc.expectTable(
    "10 characters from the Donald Duck & co. universe",
    columns,
);
if (response.outcome === "Success") {
    console.log(response.value);
}