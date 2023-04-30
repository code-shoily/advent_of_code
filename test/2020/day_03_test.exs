defmodule AdventOfCode.Y2020.Day03Test do
  @moduledoc false

  use ExUnit.Case
  @moduletag :y2003

  alias AdventOfCode.Y2020.Day03, as: Solution

  test "Year 2020, Day 3" do
    assert Solution.run() == {272, 3_898_725_600}
  end
end
