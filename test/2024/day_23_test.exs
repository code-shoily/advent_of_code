defmodule AdventOfCode.Y2024.Day23Test do
  @moduledoc false

  use ExUnit.Case, async: true
  @moduletag :y2423

  alias AdventOfCode.Y2024.Day23, as: Solution

  test "Year 2024, Day 23 run/1" do
    assert Solution.run() == {1330, "hl,io,ku,pk,ps,qq,sh,tx,ty,wq,xi,xj,yp"}
  end
end
