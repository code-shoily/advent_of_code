defmodule AdventOfCode.Y2019.Day02Test do
  @moduledoc false
  use ExUnit.Case
  @moduletag :y192

  alias AdventOfCode.Y2019.Day02, as: Solution

  test "Year 2019, Day 2, Part 1" do
    assert Solution.run_1() == 3_562_624
  end

  test "Year 2019, Day 2, Part 2" do
    assert Solution.run_2() == 8298
  end
end
