defmodule AdventOfCode.Y2015.Day22 do
  @moduledoc """
  --- Day 22: Wizard Simulator 20XX ---
  Problem Link: https://adventofcode.com/2015/day/22
  Difficulty: m
  Tags: dijkstra simulation
  """
  alias AdventOfCode.Helpers.{InputReader, Transformers}
  alias Yog.Traversal.Implicit

  def input, do: InputReader.read_from_file(2015, 22)

  def run(input \\ input()) do
    boss = parse(input)
    {solve(boss, :normal), solve(boss, :hard)}
  end

  def parse(input) do
    lines = Transformers.lines(input)

    %{
      hp: lines |> Enum.at(0) |> String.split(": ") |> List.last() |> String.to_integer(),
      dmg: lines |> Enum.at(1) |> String.split(": ") |> List.last() |> String.to_integer()
    }
  end

  defp solve(boss, mode) do
    initial_state = %{
      hp: 50,
      mana: 500,
      boss_hp: boss.hp,
      boss_dmg: boss.dmg,
      spent: 0,
      effects: %{shield: 0, poison: 0, recharge: 0},
      mode: mode
    }

    Implicit.implicit_fold_by(
      from: initial_state,
      using: :best_first,
      priority: fn state, _ -> state.spent end,
      visited_by: fn state ->
        {state.hp, state.mana, state.boss_hp, state.effects}
      end,
      successors_of: fn state ->
        if state.hp <= 0 or state.boss_hp <= 0, do: [], else: get_successors(state)
      end,
      initial: nil,
      with: fn _, state, _ ->
        if state.boss_hp <= 0, do: {:halt, state.spent}, else: {:continue, nil}
      end
    )
  end

  @spells [
    %{name: :missile, cost: 53, dmg: 4, heal: 0, effect: nil, duration: 0},
    %{name: :drain, cost: 73, dmg: 2, heal: 2, effect: nil, duration: 0},
    %{name: :shield, cost: 113, dmg: 0, heal: 0, effect: :shield, duration: 6},
    %{name: :poison, cost: 173, dmg: 0, heal: 0, effect: :poison, duration: 6},
    %{name: :recharge, cost: 229, dmg: 0, heal: 0, effect: :recharge, duration: 5}
  ]

  defp get_successors(state) do
    # Player Turn Start
    state = if state.mode == :hard, do: %{state | hp: state.hp - 1}, else: state

    if state.hp <= 0 do
      []
    else
      {state, _} = apply_effects(state)

      if state.boss_hp <= 0 do
        [%{state | boss_hp: 0}]
      else
        # Try casting each spell
        for spell <- @spells,
            state.mana >= spell.cost,
            spell.effect == nil or Map.get(state.effects, spell.effect) <= 0 do
          cast(state, spell)
        end
        |> Enum.map(fn state_after_cast ->
          if state_after_cast.boss_hp <= 0 do
            %{state_after_cast | boss_hp: 0}
          else
            # Boss Turn
            {state_after_effects, armor} = apply_effects(state_after_cast)

            if state_after_effects.boss_hp <= 0 do
              %{state_after_effects | boss_hp: 0}
            else
              # Boss Attack
              dmg = max(1, state_after_effects.boss_dmg - armor)
              final_state = %{state_after_effects | hp: state_after_effects.hp - dmg}
              if final_state.hp <= 0, do: nil, else: final_state
            end
          end
        end)
        |> Enum.reject(&is_nil/1)
      end
    end
  end

  defp apply_effects(state) do
    %{shield: s, poison: p, recharge: r} = state.effects

    boss_hp = if p > 0, do: state.boss_hp - 3, else: state.boss_hp
    mana = if r > 0, do: state.mana + 101, else: state.mana
    armor = if s > 0, do: 7, else: 0

    new_effects = %{
      shield: max(0, s - 1),
      poison: max(0, p - 1),
      recharge: max(0, r - 1)
    }

    {%{state | boss_hp: boss_hp, mana: mana, effects: new_effects}, armor}
  end

  defp cast(state, spell) do
    state = %{state | mana: state.mana - spell.cost, spent: state.spent + spell.cost}

    if spell.effect do
      %{state | effects: Map.put(state.effects, spell.effect, spell.duration)}
    else
      %{state | boss_hp: state.boss_hp - spell.dmg, hp: state.hp + spell.heal}
    end
  end
end
