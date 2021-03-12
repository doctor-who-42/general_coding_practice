#Tu use a coloured version, make sure coloredlogs is installed and uncomment selected lines

import os
import logging
from datetime import datetime
from time import time
#import coloredlogs

LOG_FILE = os.getcwd() + "/logs"
if not os.path.exists(LOG_FILE):
    os.makedirs(LOG_FILE)
LOG_FILE = LOG_FILE + "/" + datetime.fromtimestamp(time()).strftime('%Y_%m_%d %H_%M_%S') + ".log"
#logFormatter = coloredlogs.ColoredFormatter("%(levelname)-.3s %(processName)-.20s (%(asctime)s) %(message)s")
log_formatter = logging.Formatter("%(levelname)-.3s %(processName)-.20s (%(asctime)s) %(message)s")
fh = logging.FileHandler("{0}".format(LOG_FILE))
fh.setFormatter(log_formatter)
rootLogger = logging.getLogger()
rootLogger.addHandler(fh)
rootLogger.setLevel(logging.DEBUG)
logger = logging.getLogger(__name__)

#fileHandler.setFormatter(logFormatter)
#logger.addHandler(fileHandler)
#coloredlogs.install(level='DEBUG')

logger.info("Some info")
logger.debug("Some debugging stuff")
logger.warning("Some warning")
logger.error("Some problem")
logger.critical("A critical issue")
