import { Column, Table } from "../model.ts";
import { tangibleClient } from "./common.ts";

const tc = tangibleClient();

const seaAnimalColumn: Column = {
    type: "TextColumn",
    name: "Sea animal",
};
const animalTypeColumn: Column = {
    type: "EnumColumn",
    name: "Type",
    options: ["Fish", "Shellfish", "Jellyfish"],
};

const originalColumns: Column[] = [seaAnimalColumn, animalTypeColumn];

const originalTable: Table = {
    columns: originalColumns,
    rows: [
        {
            cells: [
                { type: "TextCell", value: "Cod", column: seaAnimalColumn },
                {
                    type: "EnumCell",
                    value: "Fish",
                    column: animalTypeColumn,
                },
            ],
        },
        {
            cells: [
                {
                    type: "TextCell",
                    value: "Salmon",
                    column: seaAnimalColumn,
                },
                {
                    type: "EnumCell",
                    value: "Fish",
                    column: animalTypeColumn,
                },
            ],
        },
        {
            cells: [
                {
                    type: "TextCell",
                    value: "Crab",
                    column: seaAnimalColumn,
                },
                {
                    type: "EnumCell",
                    value: "Shellfish",
                    column: animalTypeColumn,
                },
            ],
        },
        {
            cells: [
                {
                    type: "TextCell",
                    value: "Lobster",
                    column: seaAnimalColumn,
                },
                {
                    type: "EnumCell",
                    value: "Shellfish",
                    column: animalTypeColumn,
                },
            ],
        },
    ],
};

const addedColumnResponse = await tc.expectTableWithAddedColumn(
    { type: "NumberColumn", name: "Average weight" },
    "Average weight (kg)",
    originalTable,
);
if (addedColumnResponse.outcome === "Success") {
    console.log(addedColumnResponse.value);
}

//const addedRowResponse = await tc.expectTableWithAddedRow(
//    originalTable,
//    "Another random sea animal",
//);
//if (addedRowResponse.outcome === "Success") {
//    console.log(addedRowResponse.value);
//}