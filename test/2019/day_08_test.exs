defmodule AdventOfCode.Y2019.Day08Test do
  @moduledoc false
  use ExUnit.Case

  alias AdventOfCode.Y2019.Day08, as: Solution

  test "Year 2019, Day 8, Part 1" do
    assert Solution.run_1() == 1572
  end

  @tag :skip_fixme
  test "Year 2019, Day 8, Part 2" do
    # !FIXME "KYHFE"
    assert Solution.run_2() == :ok
  end
end
