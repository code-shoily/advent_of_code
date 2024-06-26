defmodule AdventOfCode.Y2023.Day19 do
  @moduledoc """
  --- Day 19: Aplenty ---
  Problem Link: https://adventofcode.com/2023/day/19
  Difficulty: l
  Tags: agent op-code parse-heavy
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2023, 19)

  def run(input \\ input()) do
    input = parse(input)

    {run_1(input), run_2(input)}
  end

  def parse(data \\ input()) do
    [workflows, ratings] =
      data
      |> Transformers.sections()
      |> Enum.map(&Transformers.lines/1)

    parsed_workflows = Map.new(workflows, &parse_workflow/1)

    parsed_ratings =
      Enum.map(ratings, fn rating ->
        rating
        |> String.slice(1..-2//1)
        |> String.split(",", trim: true)
        |> Stream.map(&String.split(&1, "=", trim: true))
        |> Stream.map(fn [_, v] -> String.to_integer(v) end)
        |> Stream.zip(~w(x m a s))
        |> Map.new(fn {v, c} -> {c, v} end)
      end)

    {parsed_workflows, parsed_ratings}
  end

  def run_1({workflows, ratings}) do
    ratings
    |> Stream.map(&{&1, process(workflows["in"], &1, workflows)})
    |> Stream.filter(&(elem(&1, 1) == :A))
    |> Stream.map(fn {%{"x" => x, "m" => m, "a" => a, "s" => s}, _} -> x + m + a + s end)
    |> Enum.sum()
  end

  defp process([{:jump, "A"} | _], _, _), do: :A
  defp process([{:jump, "R"} | _], _, _), do: :R

  defp process([{:jump, next} | _], rating, workflows),
    do: process(workflows[next], rating, workflows)

  defp process([{:conditional, c, v, cmp, o} | rest], rating, workflows) do
    (((cmp == :lt and rating[c] < v) or
        (cmp == :gt and rating[c] > v)) &&
       process([{:jump, o}], rating, workflows)) ||
      process(rest, rating, workflows)
  end

  def run_2({workflows, _}) do
    {:ok, agent} = Agent.start_link(fn -> [] end)

    tree(workflows["in"], workflows, [], agent)

    Agent.get(agent, fn content ->
      content
      |> Enum.map(fn steps ->
        steps
        |> Enum.reduce(Map.new(~w(x m a s), &{&1, 1..4000}), &range_reducer/2)
        |> Enum.reduce(1, &range_combination/2)
      end)
      |> Enum.sum()
    end)
  end

  defp range_reducer({c, :lt, v}, acc), do: Map.update!(acc, c, fn l.._//_ -> l..(v - 1)//1 end)
  defp range_reducer({c, :gt, v}, acc), do: Map.update!(acc, c, fn _..u//_ -> (v + 1)..u//1 end)
  defp range_reducer({c, :lte, v}, acc), do: Map.update!(acc, c, fn l.._//_ -> l..v//1 end)
  defp range_reducer({c, :gte, v}, acc), do: Map.update!(acc, c, fn _..u//_ -> v..u//1 end)
  defp range_combination({_, l..u//_}, acc), do: acc * (u - l + 1)

  defp parse_workflow(workflow) do
    [_, name, instructions] = Regex.run(~r/(\w+)\{(.+)\}/, workflow)

    steps =
      instructions
      |> String.split(",", trim: true)
      |> Enum.map(fn instruction ->
        if String.contains?(instruction, ":") do
          [condition, o] = String.split(instruction, ":")
          [c, v] = String.split(condition, ~r/<|>/)
          {:conditional, c, String.to_integer(v), comparator(condition), o}
        else
          {:jump, instruction}
        end
      end)

    {name, steps}
  end

  defp comparator(condition), do: if(String.contains?(condition, ">"), do: :gt, else: :lt)
  defp tree([{:jump, "A"} | _], _, path, agent), do: Agent.update(agent, &(&1 ++ [path]))
  defp tree([{:jump, "R"} | _], _, _, _), do: nil

  defp tree([{:jump, next} | _], workflows, path, agent),
    do: tree(workflows[next], workflows, path, agent)

  defp tree([{:conditional, c, v, cmp, o} | rest], workflows, path, agent) do
    if o == "A" do
      Agent.update(agent, &(&1 ++ [path ++ [{c, cmp, v}]]))
    end

    if o != "R" and o != "A" do
      tree(workflows[o], workflows, path ++ [{c, cmp, v}], agent)
    end

    tree(rest, workflows, path ++ [{c, inverse_of(cmp), v}], agent)
  end

  defp inverse_of(:lt), do: :gte
  defp inverse_of(:gt), do: :lte
end
