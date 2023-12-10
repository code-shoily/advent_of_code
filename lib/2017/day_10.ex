defmodule AdventOfCode.Y2017.Day10 do
  @moduledoc """
  --- Day 10: Knot Hash ---
  Problem Link: https://adventofcode.com/2017/day/10
  Difficulty: s
  Tags: hash
  """
  require Bitwise

  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2017, 10)

  @lst 0..255
  @size Enum.count(@lst)
  @suffix_lengths [17, 31, 73, 47, 23]

  def run(input \\ input()) do
    {lengths, bytes, list_map} = parse(input)
    {run_1(lengths, list_map), run_2(bytes, list_map)}
  end

  @doc """
  Computes knot hash for given `key_string`

  ## Example

      iex> Solution.compute_knot_hash("")
      "a2582a3a0e66e6e86e3812dcb672a272"

      iex> Solution.compute_knot_hash("AoC 2017")
      "33efeb34ea91902bb2f59c9920caa6cd"

      iex> Solution.compute_knot_hash("1,2,3")
      "3efbe78a8d82f29979031a4aa0b16a9d"

      iex> Solution.compute_knot_hash("1,2,4")
      "63960835bcdc130f0b66d7ff4f6a5a8e"

  """
  def compute_knot_hash(key_string) do
    {bytes, list_map} = parse_bytes(key_string)
    run_2(bytes, list_map)
  end

  def parse(data) do
    {bytes, list_map} = parse_bytes(data)
    {Transformers.int_words(data, ","), bytes, list_map}
  end

  def parse_bytes(data) do
    {
      String.to_charlist(data) ++ @suffix_lengths,
      to_map_list(@lst)
    }
  end

  defp run_1(lengths, list_map) do
    lengths
    |> knot_hash(list_map, 0, 0)
    |> then(fn {list_map, _, _} -> list_map end)
    |> Map.take([0, 1])
    |> Map.values()
    |> Enum.product()
  end

  defp run_2(bytes, list_map) do
    1..64
    |> Enum.reduce({list_map, 0, 0}, fn _, {list_map, pos, skip} ->
      knot_hash(bytes, list_map, pos, skip)
    end)
    |> then(fn {list_map, _, _} -> list_map end)
    |> Enum.sort_by(fn {k, _} -> k end)
    |> Enum.map(fn {_, v} -> v end)
    |> to_hash()
    |> to_hex()
  end

  defp knot_hash([], list_map, pos, skip), do: {list_map, pos, skip}

  defp knot_hash([len | rest], list_map, pos, skip) do
    knot_hash(
      rest,
      reverse_slice(list_map, pos, len),
      pos + len + skip,
      skip + 1
    )
  end

  defp reverse_slice(list_map, pos, len) do
    pos..(pos + len - 1)
    |> Enum.split_with(&(&1 >= @size))
    |> then(fn {a, b} ->
      indices = b ++ Enum.map(a, &rem(&1, @size))
      Enum.zip(indices, Enum.reverse(indices))
    end)
    |> Map.new(fn {before, later} -> {later, list_map[before]} end)
    |> Map.merge(list_map, fn _, v, _ -> v end)
  end

  defp to_map_list(list) do
    list
    |> Enum.with_index(0)
    |> Map.new(fn {v, k} -> {k, v} end)
  end

  defp to_hash(bytes) do
    bytes
    |> Enum.chunk_every(16)
    |> Enum.map(fn bits -> Enum.reduce(bits, &Bitwise.bxor/2) end)
  end

  defp to_hex(hash) do
    hash
    |> Enum.map(&Integer.to_string(&1, 16))
    |> Enum.map_join(&String.pad_leading(&1, 2, "0"))
    |> String.downcase()
  end
end
