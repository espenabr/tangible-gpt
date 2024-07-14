import { tangibleClient } from "./common.ts";

const randomElement = <T>(arr: T[]) => {
    const randomIndex = Math.floor(Math.random() * arr.length);
    return arr[randomIndex];
};

const tc = tangibleClient();

type ActivityDetails = {
    activity: string;
    description: string;
    difficulty1To10: number;
};

const response = await tc.expectJson(
    "I'm bored, give me some suggestions of what to do",
    ["<activity>"],
)
    .then((r) => {
        if (r.outcome === "Success") {
            return tc.expectJson<ActivityDetails>(
                `Tell me more about ${randomElement(r.value)}`,
                {
                    activity: "<activity>",
                    description: "<description>",
                    difficulty1To10: 5,
                },
                r.history
            );
        } else {
            return r;
        }
    });

if (response.outcome === "Success") {
    console.log(response.value);
}
