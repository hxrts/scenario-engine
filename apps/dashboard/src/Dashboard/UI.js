// The only bridge to the vendored shadcn/ui components (apps/dashboard/ui) and Recharts.
export { Card as card, CardHeader as cardHeader, CardTitle as cardTitle, CardContent as cardContent } from "@/ui/card.tsx";
export { Tabs as tabs, TabsList as tabsList, TabsTrigger as tabsTrigger, TabsContent as tabsContent } from "@/ui/tabs.tsx";
export { Checkbox as checkbox } from "@/ui/checkbox.tsx";
export { Button as button } from "@/ui/button.tsx";
export { Input as input } from "@/ui/input.tsx";
export { ChartContainer as chartContainer, ChartTooltip as chartTooltip, ChartTooltipContent as chartTooltipContent, ChartLegend as chartLegend, ChartLegendContent as chartLegendContent } from "@/ui/chart.tsx";
export { LineChart as lineChart, Line as line, BarChart as barChart, Bar as bar, XAxis as xAxis, YAxis as yAxis, CartesianGrid as cartesianGrid } from "recharts";
// Grouped bars (the Compare page): the Recharts bar chart and its bar series.
export { BarChart as groupedBarChart, Bar as groupedBar } from "recharts";
// A plain element, not a shadcn component: a labelled region for the run actions.
export const region = "section";
