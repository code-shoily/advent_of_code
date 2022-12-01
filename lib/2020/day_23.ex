defmodule AdventOfCode.Y2020.Day23 do
  @moduledoc """
  --- Day 23: Crab Cups ---
  Problem Link: https://adventofcode.com/2020/day/23
  """
  def input, do: "467528193"
  @part_1_limit 100

  def run_1, do: parse() |> play() |> labels_after(1)
  def run_2, do: {:not_implemented, 2}
  def parse, do: Enum.map(String.graphemes(input()), &String.to_integer/1)

  def play(cups), do: play(cups, hd(cups), 1)
  def play(cups, _, @part_1_limit), do: cups

  def play(cups, current, move) do
    pick_ups = pick_up(cups, current)
    destination = destination(cups, pick_ups, current - 1)

    {init, [head | tail]} = Enum.split_while(cups -- pick_ups, &(&1 != destination))
    new_cups = init ++ [head | pick_ups] ++ tail

    play(new_cups, next(new_cups, current), move + 1)
  end

  def pick_up(cups, current) do
    {a, [_ | b]} = Enum.split_while(cups, &(&1 != current))
    Enum.take(b ++ a, 3)
  end

  def next(cups, current) do
    case Enum.split_while(cups, &(&1 != current)) do
      {[next | _], [_]} -> next
      {_, [_ | [next | _]]} -> next
    end
  end

  def destination(cups, pick_ups, from) do
    if from in pick_ups do
      destination(cups, pick_ups, from - 1)
    else
      {min, max} = Enum.min_max(cups)
      (from < min && destination(cups, pick_ups, max)) || from
    end
  end

  def labels_after(cups, label) do
    {a, [_ | b]} = Enum.split_while(cups, &(&1 != label))
    Enum.join(b ++ a)
  end
end
