# goblin-workshop

## 简介

*哥布林车间*是一个并行处理AOV (Activity On Vertex) 网络的类库，适合处理若干存在复杂依赖关系的一次性任务。

比如我们有5个任务：
1. 耗时10秒
1. 耗时6秒
1. 耗时10秒
1. 耗时6秒
1. 耗时2秒

若要求：
1. 任务3依赖任务1和2
1. 任务4依赖任务2
1. 任务5依赖任务3和4

则哥布林车间的处理流程为：
1. 并行启动任务1和2；
1. 当任务2结束时，启动任务4，此时任务1和任务4并行执行；
1. 当任务1结束时，启动任务3，此时任务3和任务4并行执行；
1. 任务4结束；
1. 任务3结束；
1. 任务5开始；
1. 任务5结束。

一个简单输出的实现请参考[app/Examples/1.hs](app/Examples/1.hs)，执行结果如下：

```
/==========
| Total 5 tasks
|   - 1
|   - 2
|   - 3 (dependent on 1, 2)
|   - 4 (dependent on 2)
|   - 5 (dependent on 3, 4)
\==========
[2017-06-02 15:46:19 CST : gw.d : INFO] Dispatcher launched
[2017-06-02 15:46:19 CST : gw.s : INFO] Scheduler launched
> Starting Task 2
> Starting Task 1
* Task 2 is working
* Task 1 is working
* Task 1 is working
* Task 2 is working
* Task 1 is working
< Stopping Task 2
* Task 1 is working
> Starting Task 4
* Task 4 is working
* Task 1 is working
* Task 4 is working
< Stopping Task 1
* Task 4 is working
> Starting Task 3
* Task 3 is working
< Stopping Task 4
* Task 3 is working
* Task 3 is working
* Task 3 is working
* Task 3 is working
< Stopping Task 3
> Starting Task 5
* Task 5 is working
* Task 5 is working
* Task 5 is working
* Task 5 is working
< Stopping Task 5
[2017-06-02 15:46:41 CST : gw.d : INFO] All tasks are done. Asking scheduler to terminate
[2017-06-02 15:46:41 CST : gw.d : INFO] Bye
[2017-06-02 15:46:41 CST : gw.s : INFO] Bye
```

可将1.hs中的Logger输出等级修改为DEBUG查看更为细致的输出结果。

另一个使用了终端图形界面的更加复杂的任务系统的实现请参考[app/Examples/2.hs](app/Examples/2.hs)，
执行效果如下：

![生成gif之后垃圾配色](app/Examples/2.gif)

## TODO

[项目看板](https://github.com/y-usuzumi/goblin-workshop/projects/1)
