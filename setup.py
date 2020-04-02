import setuptools

install_dependencies = [
    'Click',
    'colorama',
    'sexpdata',
    'z3-solver',
    "rpy2",
    'sqlparse',
    'PyYAML',
    'pandas',
    'numpy',
    'ordered-set',
    'python-dateutil'
]

develop_dependencies = [
    'rpy2',  # for Morpheus. TODO: This should really belong to the client package
    'lark-parser',  # for parsing
    'sphinx',  # for documentation generation
    'sqlparse',
]

setuptools.setup(
    name='squares',
    version='1.0',
    packages=setuptools.find_packages(),
    license='LICENSE',
    description='Deduction-based synthesis framework',
    install_requires=install_dependencies,
    extras_require={
        'dev': develop_dependencies
    },
    python_requires='>=3.6',
    entry_points={
        'console_scripts': [
            'squares=squares:main',
            'cubes=cubes:main',
        ],
    },
)
