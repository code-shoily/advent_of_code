defmodule AdventOfCode.Y2021.Day15Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2115

  alias AdventOfCode.Y2021.Day15, as: Solution

  test "Year 2021, Day 15" do
    assert Solution.run() == {583, 2927}
  end

end
