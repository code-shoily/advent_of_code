defmodule AdventOfCode.Y2020.Day16 do
  @moduledoc """
  --- Day 16: Ticket Translation ---
  Problem Link: https://adventofcode.com/2020/day/16
  Difficulty: m
  Tags: range validation
  """
  alias AdventOfCode.Helpers.InputReader

  def input, do: InputReader.read_from_file(2020, 16)

  def run(input \\ input()) do
    data = parse(input)
    {run_1(data), run_2(data)}
  end

  defp run_1({rules, _, nearby}) do
    all_ranges = Enum.flat_map(rules, fn {_, rs} -> rs end)

    nearby
    |> List.flatten()
    |> Enum.filter(fn val ->
      Enum.all?(all_ranges, fn range -> val not in range end)
    end)
    |> Enum.sum()
  end

  defp run_2({rules, my_ticket, nearby}) do
    all_ranges = Enum.flat_map(rules, fn {_, rs} -> rs end)

    valid_tickets =
      Enum.filter(nearby, fn ticket ->
        Enum.all?(ticket, fn val ->
          Enum.any?(all_ranges, fn range -> val in range end)
        end)
      end)

    # Columns: lists of values for each field position
    columns =
      [my_ticket | valid_tickets]
      |> Enum.zip()
      |> Enum.map(&Tuple.to_list/1)

    # For each column, find rules that match all its values
    column_potentials =
      for {col_vals, col_idx} <- Enum.with_index(columns) do
        matches =
          for {field, rs} <- rules,
              Enum.all?(col_vals, fn v -> Enum.any?(rs, &(v in &1)) end) do
            field
          end

        {col_idx, matches}
      end

    mapping = deduce(column_potentials, %{})

    mapping
    |> Enum.filter(fn {_, field} -> String.starts_with?(field, "departure") end)
    |> Enum.map(fn {idx, _} -> Enum.at(my_ticket, idx) end)
    |> Enum.product()
  end

  defp deduce([], acc), do: acc

  defp deduce(potentials, acc) do
    # Find column with only one potential field
    {idx, [field]} = Enum.find(potentials, fn {_, ms} -> length(ms) == 1 end)

    # Remove this field from other columns' possibilities
    new_potentials =
      potentials
      |> Enum.reject(fn {i, _} -> i == idx end)
      |> Enum.map(fn {i, ms} -> {i, List.delete(ms, field)} end)

    deduce(new_potentials, Map.put(acc, idx, field))
  end

  def parse(input) do
    [rules_raw, my_raw, nearby_raw] = String.split(input, ["\n\n", "\r\n\r\n"], trim: true)

    rules =
      for line <- String.split(rules_raw, "\n", trim: true) do
        [field, ranges_raw] = String.split(line, ": ", trim: true)

        ranges =
          Regex.scan(~r/\d+\-\d+/, ranges_raw)
          |> List.flatten()
          |> Enum.map(fn r ->
            [s, e] = String.split(r, "-", trim: true)
            String.to_integer(s)..String.to_integer(e)
          end)

        {field, ranges}
      end

    {rules, parse_ticket(my_raw),
     nearby_raw |> String.split("\n", trim: true) |> Enum.drop(1) |> Enum.map(&parse_ticket/1)}
  end

  defp parse_ticket(line) do
    line
    |> String.split([",", "\n"], trim: true)
    |> Enum.filter(&Regex.match?(~r/^\d+$/, &1))
    |> Enum.map(&String.to_integer/1)
  end
end
