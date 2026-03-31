defmodule AdventOfCode.Y2018.Day07Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y1807

  alias AdventOfCode.Y2018.Day07, as: Solution

  test "Year 2018, Day 7 run/1" do
    assert Solution.run() == {"BCADPVTJFZNRWXHEKSQLUYGMIO", 973}
  end
end
