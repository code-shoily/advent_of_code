defmodule AdventOfCode.Y2022.Day15 do
  @moduledoc """
  --- Day 15: Beacon Exclusion Zone ---
  Problem Link: https://adventofcode.com/2022/day/15
  Difficulty: xl
  Tags: geometry interval-merging optimization
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2022, 15)

  def run(input \\ input()) do
    sensors = parse(input)

    run_1_task = Task.async(fn -> solve_1(sensors) end)
    run_2_task = Task.async(fn -> solve_2(sensors) end)

    {Task.await(run_1_task, :infinity), Task.await(run_2_task, :infinity)}
  end

  def parse(data) do
    data
    |> Transformers.lines()
    |> Enum.map(fn line ->
      [sx, sy, bx, by] =
        Regex.scan(~r/-?\d+/, line)
        |> List.flatten()
        |> Enum.map(&String.to_integer/1)

      d = abs(sx - bx) + abs(sy - by)
      {{sx, sy}, {bx, by}, d}
    end)
  end

  def solve_1(sensors) do
    y_target = 2_000_000

    intervals =
      sensors
      |> Enum.map(fn {{sx, sy}, _, d} ->
        dy = abs(sy - y_target)
        if dy <= d, do: {sx - (d - dy), sx + (d - dy)}, else: nil
      end)
      |> Enum.reject(&is_nil/1)

    merged = merge_intervals(intervals)
    total_covered = Enum.reduce(merged, 0, fn {x1, x2}, acc -> acc + (x2 - x1 + 1) end)

    # Count unique beacons on this row that are covered
    beacons_on_row_count =
      sensors
      |> Enum.map(fn {_, {bx, by}, _} -> if by == y_target, do: bx, else: nil end)
      |> Enum.reject(&is_nil/1)
      |> Enum.uniq()
      |> Enum.count(fn bx -> Enum.any?(merged, fn {x1, x2} -> bx >= x1 and bx <= x2 end) end)

    total_covered - beacons_on_row_count
  end

  def solve_2(sensors) do
    limit = 4_000_000

    # The gap must be formed by the intersection of lines just outside the ranges
    # Lines are: x + y = c OR x - y = c
    # For each sensor {sx, sy, d}, boundaries are at distance d+1
    # x + y = sx + sy +/- (d + 1)
    # x - y = sx - sy +/- (d + 1)

    pos_lines =
      Enum.flat_map(sensors, fn {{sx, sy}, _, d} ->
        [sx + sy + d + 1, sx + sy - d - 1]
      end)
      |> Enum.uniq()

    neg_lines =
      Enum.flat_map(sensors, fn {{sx, sy}, _, d} ->
        [sx - sy + d + 1, sx - sy - d - 1]
      end)
      |> Enum.uniq()

    # Find intersection our distress beacon is located at
    result =
      Enum.find_value(pos_lines, fn p ->
        Enum.find_value(neg_lines, fn n ->
          # x + y = p
          # x - y = n
          # 2x = p + n => x = (p + n) / 2
          # 2y = p - n => y = (p - n) / 2
          if Integer.mod(p + n, 2) == 0 do
            x = div(p + n, 2)
            y = div(p - n, 2)

            if x >= 0 and x <= limit and y >= 0 and y <= limit do
              if not Enum.any?(sensors, fn {{sx, sy}, _, d} ->
                   abs(x - sx) + abs(y - sy) <= d
                 end) do
                x * 4_000_000 + y
              end
            end
          end
        end)
      end)

    result
  end

  defp merge_intervals([]), do: []

  defp merge_intervals(intervals) do
    [h | t] = Enum.sort(intervals)

    Enum.reduce(t, [h], fn {x1, x2}, [{prev1, prev2} | rest] ->
      if x1 <= prev2 + 1 do
        [{prev1, max(prev2, x2)} | rest]
      else
        [{x1, x2}, {prev1, prev2} | rest]
      end
    end)
    |> Enum.reverse()
  end
end
