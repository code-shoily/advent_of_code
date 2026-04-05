defmodule AdventOfCode.Y2016.Day05 do
  @moduledoc """
  --- Day 5: How About a Nice Game of Chess? ---
  Problem Link: https://adventofcode.com/2016/day/5
  Difficulty: s
  Tags: binary md5 slow
  """
  def input, do: "cxdnnyjw"

  def run(input \\ input()) do
    matches =
      Stream.iterate(0, &(&1 + 50000))
      |> Task.async_stream(
        fn start ->
          find_matches_in_range(input, start, start + 49999)
        end,
        max_concurrency: System.schedulers_online(),
        timeout: :infinity
      )
      |> Stream.flat_map(fn {:ok, res} -> res end)

    part_1 =
      matches
      |> Stream.map(fn {_idx, hash} ->
        <<_::binary-size(2), _::4, char::4, _::bitstring>> = hash
        Integer.to_string(char, 16) |> String.downcase()
      end)
      |> Stream.take(8)
      |> Enum.join("")

    part_2 =
      matches
      |> Stream.transform(MapSet.new(), fn {_, hash}, done ->
        <<_::binary-size(2), _::4, pos::4, val::4, _::bitstring>> = hash

        if pos < 8 and not MapSet.member?(done, pos) do
          {[{pos, Integer.to_string(val, 16) |> String.downcase()}], MapSet.put(done, pos)}
        else
          {[], done}
        end
      end)
      |> Stream.take(8)
      |> Enum.sort_by(&elem(&1, 0))
      |> Enum.map_join(&elem(&1, 1))

    {part_1, part_2}
  end

  defp find_matches_in_range(input, start, stop) do
    Enum.reduce(start..stop, [], fn i, acc ->
      hash = :crypto.hash(:md5, input <> Integer.to_string(i))

      case hash do
        <<0, 0, 0::4, _::4, _::binary>> -> [{i, hash} | acc]
        _ -> acc
      end
    end)
    |> Enum.reverse()
  end
end
