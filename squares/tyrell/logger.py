import logging
import time

import click

start = time.time()

class _ColorFormatter(logging.Formatter):
    colors = {
        'error': dict(fg='red'),
        'exception': dict(fg='red'),
        'critical': dict(fg='red'),
        'debug': dict(fg='green'),
        'info': dict(fg='blue'),
        'warning': dict(fg='yellow')
    }

    def __init__(self):
        self.id = 'main'
        super().__init__()

    def format(self, record):
        if not record.exc_info:
            level = record.levelname.lower()
            msg = record.getMessage()
            if level in self.colors:
                id = f'[{str(round(time.time() - start, 5)).ljust(9)}][{self.id}]'
                prefix = click.style(f'[{level}] ',
                                     **self.colors[level])
                msg = '\n'.join(id + prefix + x for x in msg.splitlines())
            return msg
        return logging.Formatter.format(self, record)


class _ClickHandler(logging.Handler):
    _use_stderr = True

    def emit(self, record):
        try:
            msg = self.format(record)
            click.echo(msg, err=self._use_stderr)
        except Exception:
            self.handleError(record)

    def set_identifier(self, id: str):
        self.formatter.id = id


_click_handler = _ClickHandler()
_click_handler.formatter = _ColorFormatter()


def get_logger(name):
    '''Return a colorful logger with the given name'''
    logger = logging.getLogger(name)
    logger.handlers = [_click_handler]
    logger.propagate = False
    return logger
