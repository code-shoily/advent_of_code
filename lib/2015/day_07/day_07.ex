defmodule AdventOfCode.Y2015.Day07 do
  @moduledoc """
  --- Day 7: Some Assembly Required ---
  Problem Link: https://adventofcode.com/2015/day/7
  """
  alias AdventOfCode.Helpers.InputReader
  alias AdventOfCode.Y2015.Day07.ControlPanel

  def input, do: InputReader.read_from_file(2015, 7)

  @bin_ops ~w/RSHIFT LSHIFT AND OR/

  def run(input \\ input()) do
    parsed_input = parse(input)
    solution_1 = run_1(parsed_input)
    solution_2 = run_2(parsed_input, solution_1)

    {solution_1, solution_2}
  end

  defp run_1(parsed_input) do
    process_and_get_signal(parsed_input, "a")
  end

  defp run_2({assignments, relations}, override) do
    assignments
    |> Enum.map(fn
      {"b", assignment} -> {"b", Map.merge(assignment, %{relation: {:assign, override}})}
      assignment -> assignment
    end)
    |> then(fn assignments -> {assignments, relations} end)
    |> process_and_get_signal("a")
  end

  def parse(data \\ input()) do
    data
    |> String.split("\n", trim: true)
    |> Enum.map(&tokenize/1)
    |> Map.new(fn {instruction, wire} ->
      {wire, %{relation: instruction, providers: get_providers(instruction)}}
    end)
    |> Enum.split_with(fn
      {_, %{providers: []}} -> true
      _ -> false
    end)
  end

  # --- <Solution Functions> ---
  defp process_and_get_signal({assignments, relations} = instructions, wire) do
    ControlPanel.start_servers()

    ControlPanel.create_wires(instructions)
    ControlPanel.build_dependencies(relations)
    ControlPanel.provide_signals(assignments)

    result = ControlPanel.get_signal(wire)

    ControlPanel.stop_servers()

    result
  end

  defp tokenize(line) do
    line
    |> String.split("->")
    |> then(fn [lhs, rhs] ->
      {parse_instruction(lhs), String.trim(rhs)}
    end)
  end

  defp get_providers(relation) do
    case relation do
      {_, value} when is_binary(value) ->
        [value]

      {_, value_a, value_b} when is_binary(value_a) and is_binary(value_b) ->
        [value_a, value_b]

      {_, value, _} when is_binary(value) ->
        [value]

      {_, _, value} when is_binary(value) ->
        [value]

      _ ->
        []
    end
  end

  defp parse_instruction(s) do
    case Regex.split(~r/\s+/, s, trim: true) do
      ["NOT", value] ->
        {:not, sanitize(value)}

      [value_1, op, value_2] when op in @bin_ops ->
        {String.to_atom(String.downcase(op)), sanitize(value_1), sanitize(value_2)}

      [value] ->
        {:assign, sanitize(value)}
    end
  end

  defp sanitize(value) do
    case Integer.parse(value) do
      {value, ""} -> value
      :error -> value
    end
  end

  # --- </Solution Functions> ---
end
