// The only bridge to the vendored shadcn/ui components (apps/dashboard/ui) and Recharts.
export { Card as card, CardHeader as cardHeader, CardTitle as cardTitle, CardContent as cardContent } from "@/ui/card.tsx";
export { Tabs as tabs, TabsList as tabsList, TabsTrigger as tabsTrigger, TabsContent as tabsContent } from "@/ui/tabs.tsx";
export { Checkbox as checkbox } from "@/ui/checkbox.tsx";
export { Button as button } from "@/ui/button.tsx";
export { Input as input } from "@/ui/input.tsx";
export { ChartContainer as chartContainer, ChartTooltip as chartTooltip, ChartTooltipContent as chartTooltipContent, ChartLegend as chartLegend, ChartLegendContent as chartLegendContent } from "@/ui/chart.tsx";
export { LineChart as lineChart, Line as line, XAxis as xAxis, YAxis as yAxis, CartesianGrid as cartesianGrid } from "recharts";
export { Sheet as sheet, SheetTrigger as sheetTrigger, SheetContent as sheetContent, SheetHeader as sheetHeader, SheetTitle as sheetTitle } from "@/ui/sheet.tsx";
export { SidebarProvider as sidebarProvider, Sidebar as sidebar, SidebarHeader as sidebarHeader, SidebarContent as sidebarContent, SidebarGroup as sidebarGroup, SidebarMenu as sidebarMenu, SidebarMenuItem as sidebarMenuItem, SidebarMenuButton as sidebarMenuButton, SidebarInset as sidebarInset, SidebarTrigger as sidebarTrigger } from "@/ui/sidebar.tsx";
export { Separator as separator } from "@/ui/separator.tsx";
// A plain element, not a shadcn component: a labelled region for the run actions.
export const region = "section";
