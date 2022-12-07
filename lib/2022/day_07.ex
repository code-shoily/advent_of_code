defmodule AdventOfCode.Y2022.Day07 do
  @moduledoc """
  --- Day 7: No Space Left On Device ---
  Problem Link: https://adventofcode.com/2022/day/7
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2022, 7)

  def run(input \\ input()) do
    directories = parse(input)
    {run_1(directories), run_2(directories, Enum.max(directories))}
  end

  defp run_1(sizes), do: Enum.sum(Enum.filter(sizes, &(&1 <= 100_000)))
  defp run_2(sizes, tot), do: Enum.min(Enum.filter(sizes, &space_required(&1, tot)))
  def parse(data \\ input()), do: Map.values(dir_sizes(Transformers.lines(data)))

  defp dir_sizes(commands) do
    {_, _, files} =
      Enum.reduce(commands, {[], "/", []}, fn cmd, {path, cur, files} = acc ->
        case cmd do
          "$ cd /" ->
            acc

          "$ cd .." ->
            {tl(path), hd(path), files}

          "$ cd " <> dir ->
            {[cur | path], dir, files}

          "$" <> _ ->
            acc

          "dir" <> _ ->
            acc

          file ->
            [size, file] = Transformers.words(file)
            {path, cur, [{file, String.to_integer(size), dirs([cur | path])} | files]}
        end
      end)

    Enum.reduce(files, %{}, fn {_, size, parents}, dirs ->
      Enum.reduce(parents, dirs, fn parent, acc -> update_dir_size(acc, parent, size) end)
    end)
  end

  defp update_dir_size(dirs, dir, size), do: Map.update(dirs, dir, size, &(size + &1))
  defp dirs(dirs), do: dirs(dirs, [])
  defp dirs([], res), do: res
  defp dirs([_ | ancestors] = path, res), do: dirs(ancestors, [path | res])
  defp space_required(cur, tot), do: cur >= 30_000_000 - (70_000_000 - tot)
end
