import chromalog
import logging


class TyrellLogFormatter(chromalog.ColorizingFormatter):

    def format(self, record):
        record.seconds = "{:10.3f}".format(record.relativeCreated / 1000)
        return super().format(record)


def setup_logger(name: str):
    logger = logging.getLogger(name)
    formatter = TyrellLogFormatter(fmt='[%(seconds)s][%(processName)s][%(levelname)s] %(message)s')
    handler = chromalog.ColorizingStreamHandler()
    handler.setFormatter(formatter)
    logger.addHandler(handler)
