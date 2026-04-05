defmodule AdventOfCode.Y2022.Day11 do
  @moduledoc """
  --- Day 11: Monkey in the Middle ---
  Problem Link: https://adventofcode.com/2022/day/11
  Difficulty: l
  Tags: math parsing
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}

  def input, do: InputReader.read_from_file(2022, 11)

  def run(input \\ input()) do
    monkeys = parse(input)
    {solve1(monkeys), solve2(monkeys)}
  end

  defp solve1(monkeys) do
    monkeys
    |> simulate(20, fn w -> div(w, 3) end)
    |> monkey_business()
  end

  defp solve2(monkeys) do
    common_mod = monkeys |> Map.values() |> Enum.map(& &1.test) |> Enum.reduce(1, &*/2)

    monkeys
    |> simulate(10_000, fn w -> rem(w, common_mod) end)
    |> monkey_business()
  end

  defp simulate(monkeys, rounds, relax_fn) do
    ids = Map.keys(monkeys) |> Enum.sort()

    Enum.reduce(1..rounds, monkeys, fn _, monkeys_acc ->
      Enum.reduce(ids, monkeys_acc, fn id, monkeys_round ->
        monkey = monkeys_round[id]

        if monkey.items == [] do
          monkeys_round
        else
          throws =
            Enum.reduce(monkey.items, %{}, fn item, throw_acc ->
              new_worry = monkey.op.(item) |> relax_fn.()

              target =
                if rem(new_worry, monkey.test) == 0,
                  do: monkey.true_target,
                  else: monkey.false_target

              Map.update(throw_acc, target, [new_worry], fn current -> current ++ [new_worry] end)
            end)

          Enum.reduce(throws, monkeys_round, fn {target_id, items}, ms ->
            update_in(ms, [target_id, :items], fn current -> current ++ items end)
          end)
          |> update_in([id, :inspected], &(&1 + length(monkey.items)))
          |> put_in([id, :items], [])
        end
      end)
    end)
  end

  defp monkey_business(monkeys) do
    monkeys
    |> Map.values()
    |> Enum.map(& &1.inspected)
    |> Enum.sort(:desc)
    |> Enum.take(2)
    |> Enum.product()
  end

  defp parse(input) do
    input
    |> Transformers.sections()
    |> Enum.with_index()
    |> Enum.reduce(%{}, fn {section, idx}, acc ->
      lines = Transformers.lines(section)
      [_, items_l, op_l, test_l, t_l, f_l] = Enum.map(lines, &String.trim/1)

      monkey = %{
        items: parse_items(items_l),
        op: parse_op(op_l),
        test: parse_last_int(test_l),
        true_target: parse_last_int(t_l),
        false_target: parse_last_int(f_l),
        inspected: 0
      }

      Map.put(acc, idx, monkey)
    end)
  end

  defp parse_items(line) do
    line
    |> String.split(": ")
    |> List.last()
    |> String.split(", ")
    |> Enum.map(&String.to_integer/1)
  end

  defp parse_last_int(line) do
    line
    |> String.split(" ")
    |> List.last()
    |> String.to_integer()
  end

  defp parse_op(line) do
    [_, expr] = String.split(line, "new = ")
    [left_s, op_s, right_s] = String.split(expr, " ")

    fn old ->
      l = if left_s == "old", do: old, else: String.to_integer(left_s)
      r = if right_s == "old", do: old, else: String.to_integer(right_s)

      case op_s do
        "+" -> l + r
        "*" -> l * r
      end
    end
  end
end
