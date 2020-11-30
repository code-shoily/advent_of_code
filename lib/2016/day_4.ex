defmodule AdventOfCode.Y2016.Day4 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2016/day/4
  """
  use AdventOfCode.Helpers.InputReader, year: 2016, day: 4

  def process(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_encrypted_name/1)
  end

  def parse_encrypted_name(name) do
    name
    |> String.split("-")
    |> Enum.split(-1)
    |> parse()
  end

  defp parse({names, [suffix]}) do
    [sector, checksum] = String.split(suffix |> String.replace("]", ""), "[")

    %{
      names: names,
      sector: String.to_integer(sector),
      checksum: checksum
    }
  end

  defp real_room?(%{names: names, checksum: checksum}) do
    names
    |> Enum.join()
    |> compute_checksum()
    |> Kernel.==(checksum)
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

  def rotate('-', _), do: " "

  def rotate(char, by) do
    ?a..?z
    |> Stream.cycle()
    |> Stream.take(by + rem(hd(char), 97) + 1)
    |> Enum.reverse()
    |> hd()
    |> List.wrap()
    |> to_string()
  end

  def run_1 do
    input!()
    |> process()
    |> Enum.filter(&real_room?/1)
    |> Enum.reduce(0, fn %{sector: sector}, acc -> sector + acc end)
  end

  @key "northpole object storage"
  def run_2 do
    input!()
    |> process()
    |> Enum.map(fn %{names: names, sector: sector} ->
      names
      |> Enum.join("-")
      |> String.to_charlist()
      |> Enum.map(&rotate([&1], sector))
      |> Enum.join()
      |> (fn name -> {name, sector} end).()
    end)
    |> Enum.into(%{})
    |> Map.get(@key)
  end

  def run, do: {run_1(), run_2()}
end
