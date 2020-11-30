alias AdventOfCode.Y2015.Day7

defmodule Day7.Stack do
  alias __MODULE__

  defstruct [:data]

  def new do
    %Stack{data: :queue.new()}
  end

  def push(%Stack{data: data}, value) do
    %Stack{data: :queue.in(value, data)}
  end

  def pop(%Stack{data: data}) do
    case :queue.out_r(data) do
      {{:value, value}, modified} -> {value, %Stack{data: modified}}
      {:empty, stack} -> {:empty, stack}
    end
  end

  def empty?(%Stack{data: data}), do: :queue.is_empty(data)
  def size(%Stack{data: data}), do: :queue.len(data)
end

defmodule Day7.Store do
  use Agent

  alias Day7.{Stack, Store}

  def start_link(table) do
    Agent.start_link(fn -> %{stack: Stack.new(), vars: [], table: table} end)
  end

  def get(pid, key) do
    %{table: table} = Agent.get(pid, & &1)
    table[key]
  end

  def run(pid, key) do
    %{table: table} = Agent.get(pid, & &1)
  end
end

defmodule Day7 do
  @moduledoc """
  Problem Link: https://adventofcode.com/2015/day/7
  """
  alias __MODULE__.Store

  use Bitwise

  use AdventOfCode.Data.InputReader, year: 2015, day: 7

  @operators ~w/AND OR NOT LSHIFT RSHIFT/

  @type operand :: integer() | atom()

  def process(input) do
    input
    |> String.split("\n", trim: true)
    |> Enum.map(&parse_line/1)
    |> Map.new()
  end

  def run, do: {run_1(), run_2()}

  def run_1() do
    {:ok, pid} = Store.start_link(input!() |> process())
    Store.run(pid, :a)
  end

  def run_2 do
    {:not_implemented, 2}
  end

  @assignment " -> "
  defp parse_line(command) do
    command
    |> String.split(@assignment)
    |> (fn [lhs, rhs] -> {String.to_atom(rhs), parse(lhs)} end).()
  end

  defp parse(lhs) when is_binary(lhs) do
    tokens = String.split(lhs, " ")
    parse(tokens)
  end

  defp parse([operator]), do: var_or_val(operator)

  defp parse([operator, operand]), do: {command(operator), var_or_val(operand)}

  defp parse([operand_1, operator, operand_2]),
    do: {command(operator), var_or_val(operand_1), var_or_val(operand_2)}

  defp parse(_), do: {}

  defp command(operator) when operator in @operators do
    operator |> String.downcase() |> String.to_atom()
  end

  defp var_or_val(op) do
    case Integer.parse(op) do
      {value, _} -> value
      :error -> String.to_atom(op)
    end
  end
end
