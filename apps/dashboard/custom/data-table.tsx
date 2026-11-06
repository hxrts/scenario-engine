// shadcn/ui data table recipe (https://ui.shadcn.com/docs/components/data-table) over
// TanStack Table: sortable columns over string rows; `link` makes a row's first cell a link
// and `mark` highlights a cell.
import * as React from "react"
import { flexRender, getCoreRowModel, getSortedRowModel, useReactTable, type ColumnDef, type SortingState } from "@tanstack/react-table"
import { Table, TableBody, TableCell, TableHead, TableHeader, TableRow } from "@/ui/table"
import { Button } from "@/ui/button"
import { ArrowUpDown } from "lucide-react"

type Props = {
  columns: string[]
  rows: string[][]
  link?: (row: string[]) => string | null
  mark?: (row: string[], i: number) => boolean
}

export function DataTable({ columns, rows, link, mark }: Props) {
  const [sorting, setSorting] = React.useState<SortingState>([])
  const defs = React.useMemo<ColumnDef<string[]>[]>(() => columns.map((title, i) => ({
    id: String(i),
    accessorFn: (r) => r[i],
    header: ({ column }) => (
      <Button variant="ghost" size="sm" className="-ml-3" onClick={() => column.toggleSorting(column.getIsSorted() === "asc")}>
        {title}<ArrowUpDown className="size-3" />
      </Button>
    ),
    cell: ({ row }) => {
      const v = row.original[i]
      const href = i === 0 && link ? link(row.original) : null
      return href ? <a className="underline" href={href}>{v}</a> : v
    },
  })), [columns, link])
  const table = useReactTable({ data: rows, columns: defs, state: { sorting }, onSortingChange: setSorting, getCoreRowModel: getCoreRowModel(), getSortedRowModel: getSortedRowModel() })
  return (
    <Table>
      <TableHeader>
        {table.getHeaderGroups().map((g) => (
          <TableRow key={g.id}>{g.headers.map((h) => <TableHead key={h.id} aria-label={columns[Number(h.id)]}>{flexRender(h.column.columnDef.header, h.getContext())}</TableHead>)}</TableRow>
        ))}
      </TableHeader>
      <TableBody>
        {table.getRowModel().rows.map((r) => (
          <TableRow key={r.id}>
            {r.getVisibleCells().map((c, i) => (
              <TableCell key={c.id} className={mark?.(r.original, i) ? "bg-amber-100 font-medium" : ""}>{flexRender(c.column.columnDef.cell, c.getContext())}</TableCell>
            ))}
          </TableRow>
        ))}
      </TableBody>
    </Table>
  )
}
