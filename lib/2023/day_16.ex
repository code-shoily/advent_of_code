defmodule AdventOfCode.Y2023.Day16 do
  @moduledoc """
  --- Day 16: The Floor Will Be Lava ---
  Problem Link: https://adventofcode.com/2023/day/16
  Difficulty: l
  Tags: grid memoization
  """
  alias AdventOfCode.Algorithms.Grid
  alias AdventOfCode.Helpers.InputReader

  @north {-1, 0}
  @south {1, 0}
  @east {0, 1}
  @west {0, -1}
  @horizontal [@east, @west]
  @vertical [@north, @south]

  def input, do: InputReader.read_from_file(2023, 16)

  def run(input \\ input()) do
    input = parse(input)

    task_1 = Task.async(fn -> run_1(input) end)
    task_2 = Task.async(fn -> run_2(input) end)

    {Task.await(task_1, :infinity), Task.await(task_2, :infinity)}
  end

  defp run_1(input) do
    {0, 0}
    |> move_beam(@east, MapSet.new(), input, MapSet.new())
    |> elem(1)
    |> MapSet.size()
  end

  defp run_2(input) do
    input
    |> edges()
    |> Task.async_stream(
      fn {position, direction} ->
        position
        |> move_beam(direction, MapSet.new(), input, MapSet.new())
        |> elem(1)
        |> MapSet.size()
      end,
      ordered: false
    )
    |> Enum.reduce(0, fn {:ok, result}, acc -> max(result, acc) end)
  end

  def parse(input \\ input()) do
    grid =
      input
      |> Grid.text_to_grid2d()
      |> Map.reject(fn {_, d} -> d == "." end)

    {max_row, _} = grid |> Map.keys() |> Enum.max_by(&elem(&1, 0))
    {_, max_col} = grid |> Map.keys() |> Enum.max_by(&elem(&1, 1))
    {{0..max_row, 0..max_col}, grid}
  end

  def edges({{rrange, crange}, _}) do
    vertical = Enum.flat_map(crange, &[{{0, &1}, @south}, {{rrange.last, &1}, @north}])
    horizontal = Enum.flat_map(rrange, &[{{&1, 0}, @east}, {{&1, crange.last}, @west}])
    vertical ++ horizontal
  end

  defp move_beam(position, direction, seen, grid, energized) do
    if MapSet.member?(seen, {position, direction}) do
      {seen, energized}
    else
      seen = MapSet.put(seen, {position, direction})

      case get_grid(grid, position) do
        nil ->
          {seen, energized}

        other ->
          energized = MapSet.put(energized, position)

          Enum.reduce(
            get_beams(other, direction),
            {seen, energized},
            &move_fold(&1, &2, position, grid)
          )
      end
    end
  end

  defp move_fold(beam, {seen, energized}, position, grid),
    do:
      position
      |> Grid.add(beam)
      |> move_beam(beam, seen, grid, energized)

  defp get_beams(:empty, direction), do: List.wrap(direction)
  defp get_beams("|", direction) when direction in @horizontal, do: @vertical
  defp get_beams("|", direction), do: List.wrap(direction)
  defp get_beams("-", direction) when direction in @vertical, do: @horizontal
  defp get_beams("-", direction), do: List.wrap(direction)
  defp get_beams("/", @west), do: [@south]
  defp get_beams("/", @east), do: [@north]
  defp get_beams("/", @north), do: [@east]
  defp get_beams("/", @south), do: [@west]
  defp get_beams("\\", @west), do: [@north]
  defp get_beams("\\", @east), do: [@south]
  defp get_beams("\\", @north), do: [@west]
  defp get_beams("\\", @south), do: [@east]

  defp get_grid({{rrange, crange}, grid}, {row, col} = position),
    do: (row in rrange && col in crange && Map.get(grid, position, :empty)) || nil
end
