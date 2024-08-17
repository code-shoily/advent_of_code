defmodule AdventOfCode.Y2016.Day04 do
  @moduledoc """
  --- Day 4: Security Through Obscurity ---
  Problem Link: https://adventofcode.com/2016/day/4
  Difficulty: xs
  Tags: sequence checksum rust
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2016, 4)

  def run(input \\ input()) do
    input = parse(input)
    {run_1(input), run_2(input)}
  end

  def run_1(input) do
    input
    |> Enum.filter(&real_room?/1)
    |> Enum.reduce(0, fn %{sector: sector}, acc -> sector + acc end)
  end

  @key "northpole object storage"
  def run_2(input) do
    input
    |> Enum.map(fn %{names: names, sector: sector} ->
      names
      |> Enum.join("-")
      |> String.to_charlist()
      |> Enum.map_join(&rotate([&1], sector))
      |> then(fn name -> {name, sector} end)
    end)
    |> List.keyfind(@key, 0)
    |> elem(1)
  end

  def parse(data) do
    for line <- Transformers.lines(data), do: decrypt(line)
  end

  defp decrypt({names, [suffix]}) do
    [sector, checksum] = String.split(suffix |> String.replace("]", ""), "[")

    %{
      names: names,
      sector: String.to_integer(sector),
      checksum: checksum
    }
  end

  defp decrypt(name) do
    name
    |> String.split("-")
    |> Enum.split(-1)
    |> decrypt()
  end

  defp real_room?(%{names: names, checksum: checksum}) do
    checksum == compute_checksum(Enum.join(names))
  end

  defp compute_checksum(name) do
    name
    |> String.graphemes()
    |> Enum.sort()
    |> Enum.chunk_by(& &1)
    |> Enum.sort_by(&length/1, :desc)
    |> Enum.map(&hd/1)
    |> Enum.take(5)
    |> Enum.join()
  end

  def rotate(~c"-", _), do: " "

  def rotate(char, by) do
    ?a..?z
    |> Stream.cycle()
    |> Stream.take(by + rem(hd(char), 97) + 1)
    |> Enum.reverse()
    |> hd()
    |> List.wrap()
    |> to_string()
  end
end
