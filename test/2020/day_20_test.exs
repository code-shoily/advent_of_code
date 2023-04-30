defmodule AdventOfCode.Y2020.Day20Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2020

  alias AdventOfCode.Y2020.Day20, as: Solution

  test "Year 2020, Day 20" do
    assert Solution.run() == {29_125_888_761_511, {:todo, 2}}
  end
end
