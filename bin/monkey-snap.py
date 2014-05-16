import datetime

def snap():
    from com.android.monkeyrunner import MonkeyRunner, MonkeyDevice
    device = MonkeyRunner.waitForConnection()
    result = device.takeSnapshot()
    now = datetime.datetime.now()
    file = now.strftime("%d%m%Y-%H%M%S")+".png"
    result.writeToFile(file,'png')
    print file

snap()
