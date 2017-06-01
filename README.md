# goblin-workshop

## 简介

*哥布林车间*是一个并行处理AOE (Activity On Edge) 网络的类库，适合处理若干存在复杂依赖关系的一次性任务。

比如我们有四个任务：
1. 耗时10秒
1. 耗时6秒
1. 耗时10秒
1. 耗时6秒

若要求：
1. 任务3依赖任务1和2
1. 任务4依赖任务2

则哥布林车间的处理流程为：
1. 并行启动任务1和2；
1. 当任务2结束时，启动任务4，此时任务1和任务4并行执行；
1. 当任务1结束时，启动任务3,此时任务3和任务4并行执行；
1. 任务4结束；
1. 任务3结束。

实现请参考[app/Main.hs](app/Main.hs)：

执行结果如下：

```
/==========
| Total 4 tasks
|   - 1
|   - 2
|   - 3 (dependent on 1, 2)
|   - 4 (dependent on 2)
\==========
[2017-06-02 01:07:38 CST : gw.d : INFO] Dispatcher launched
[2017-06-02 01:07:38 CST : gw.s : INFO] Scheduler launched
Task 2 is working
Task 1 is working
Task 1 is working
Task 2 is working
Task 1 is working
Task 4 is working
Task 1 is working
Task 4 is working
Task 1 is working
Task 4 is working
Task 3 is working
Task 3 is working
Task 3 is working
Task 3 is working
Task 3 is working
[2017-06-02 01:07:58 CST : gw.d : INFO] All tasks are done. Terminating scheduler
[2017-06-02 01:07:58 CST : gw.d : INFO] Bye
[2017-06-02 01:07:58 CST : gw.s : INFO] Bye
```

可将Main.hs中的Logger输出等级修改为DEBUG查看更为细致的输出结果。
