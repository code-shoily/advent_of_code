defmodule AdventOfCode.Y2020.Day10Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2010

  alias AdventOfCode.Y2020.Day10, as: Solution

  test "Year 2020, Day 10, Part 1" do
    assert Solution.run_1() == 2030
  end

  test "Year 2020, Day 10, Part 2" do
    assert Solution.run_2() == 42_313_823_813_632
  end
end
