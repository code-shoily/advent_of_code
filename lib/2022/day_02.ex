defmodule AdventOfCode.Y2022.Day02 do
  @moduledoc """
  --- Day 2: Rock Paper Scissors ---
  Problem Link: https://adventofcode.com/2022/day/2
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  @winning_pt 6
  @draw_pt 3
  @pts %{rock: 1, paper: 2, scissor: 3}
  @to_win %{rock: :paper, paper: :scissor, scissor: :rock}
  @to_lose Map.new(@to_win, fn {a, b} -> {b, a} end)
  @fate %{"X" => &__MODULE__.lose/1, "Y" => &__MODULE__.draw/1, "Z" => &__MODULE__.win/1}

  def input, do: InputReader.read_from_file(2022, 2)

  def run(input \\ input()) do
    input = parse(input)

    {score_1(input), score_2(input)}
  end

  def parse(data \\ input()) do
    for line <- Transformers.lines(data) do
      [a, b] = Transformers.words(line)
      {to_rps(a), b}
    end
  end

  defp score_1(input), do: Enum.reduce(input, 0, fn x, tot -> tot + elem(pts(x), 1) end)

  defp score_2(input) do
    input
    |> Enum.reduce([], fn {a, b}, acc -> [{a, @fate[b].(a)} | acc] end)
    |> score_1()
  end

  def lose(a), do: @to_lose[a]
  def draw(a), do: a
  def win(a), do: @to_win[a]

  def pts({a, b}) when is_binary(b), do: pts({a, to_rps(b)})
  def pts({:scissor, :rock}), do: {3, @winning_pt + 1}
  def pts({:rock, :paper}), do: {1, @winning_pt + 2}
  def pts({:paper, :scissor}), do: {2, @winning_pt + 3}
  def pts({a, a}), do: {@draw_pt + @pts[a], @draw_pt + @pts[a]}
  def pts({a, b}), do: {@winning_pt + @pts[a], @pts[b]}

  def to_rps(a) when a in ~w/A X/, do: :rock
  def to_rps(a) when a in ~w/B Y/, do: :paper
  def to_rps(a) when a in ~w/C Z/, do: :scissor
end
