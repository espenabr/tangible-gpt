import { Column } from "../model.ts";
import { tangibleClient } from "./common.ts";

const tc = tangibleClient();

const columns: Column[] = [
    { columnType: "TextColumn", name: "Name" },
    {
        columnType: "SingleChoiceColumn",
        name: "Animal",
        options: ["Duck", "Goose", "Mouse", "Dog", "Cow"],
    },
    { columnType: "TextColumn", name: "Occupation" },
    { columnType: "TextColumn", name: "Personality" },
    { columnType: "BooleanColumn", name: "Is old" },
];

const response = await tc.expectTable(
    "10 characters from the Donald Duck & co. universe",
    columns,
);
if (response.outcome === "Success") {
    console.log(response.value);
}