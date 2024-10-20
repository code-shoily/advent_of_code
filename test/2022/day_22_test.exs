defmodule AdventOfCode.Y2022.Day22Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2222

  alias AdventOfCode.Y2022.Day22, as: Solution

  test "Year 2022, Day 22 run/1" do
    assert Solution.run() == {123_046, {:todo, 2}}
  end
end
