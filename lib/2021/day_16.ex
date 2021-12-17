defmodule AdventOfCode.Y2021.Day16 do
  @moduledoc """
  --- Day 16: Packet Decoder ---
  Problem Link: https://adventofcode.com/2021/day/16
  """
  use AdventOfCode.Helpers.InputReader, year: 2021, day: 16

  def run_1, do: input!() |> parse() |> sum()
  def run_2, do: input!() |> parse() |> eval()
  def parse(data), do: data |> Base.decode16!() |> parse_packet() |> elem(0)

  defp parse_packet(<<ver::3, 4::3, rest::bitstring>>) do
    {val, rest} = parse_literal(rest, 0)

    {{:literal, ver, val}, rest}
  end

  defp parse_packet(<<ver::3, type::3, 0::1, length::15, rest::bitstring>>) do
    <<subpackets::bitstring-size(length), rest::bitstring>> = rest

    {{type, ver, parse_packets(subpackets)}, rest}
  end

  defp parse_packet(<<ver::3, type::3, 1::1, length::11, rest::bitstring>>) do
    {val, rest} = Enum.map_reduce(1..length, rest, fn _, acc -> parse_packet(acc) end)

    {{type, ver, val}, rest}
  end

  defp parse_packets({packet, <<>>}), do: packet |> List.wrap()
  defp parse_packets({packet, rest}), do: [packet | parse_packets(rest)]
  defp parse_packets(hex), do: hex |> parse_packet |> parse_packets()

  defp parse_literal(<<1::1, bits::4, rest::bitstring>>, acc) do
    parse_literal(rest, acc * 0x10 + bits)
  end

  defp parse_literal(<<0::1, bits::4, rest::bitstring>>, acc) do
    {acc * 0x10 + bits, rest}
  end

  defp sum({:literal, ver, _}), do: ver
  defp sum({_, ver, val}), do: Enum.reduce(val, ver, &(sum(&1) + &2))
  defp eval({:literal, _, val}), do: val
  defp eval({0, _, _} = packet), do: reduce(packet, 0, &+/2)
  defp eval({1, _, _} = packet), do: reduce(packet, 1, &*/2)
  defp eval({2, _, _} = packet), do: reduce(packet, :inf, &min/2)
  defp eval({3, _, _} = packet), do: reduce(packet, 0, &max/2)
  defp eval({5, _, _} = packet), do: compare(packet, &>/2)
  defp eval({6, _, _} = packet), do: compare(packet, &</2)
  defp eval({7, _, _} = packet), do: compare(packet, &==/2)
  defp reduce({_, _, val}, x, f), do: Enum.reduce(val, x, &f.(eval(&1), &2))
  defp compare({_, _, [a, b]}, f), do: (f.(eval(a), eval(b)) && 1) || 0
end
