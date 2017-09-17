__author__ = 'schien'
import numpy as np
import seaborn as sns
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.projections.polar import PolarAxes
from matplotlib.projections import register_projection

# TODO: transpose current plot structure. Keep only results regardless of gender.

tabela_ufnome = {'11': 'Rondônia',
                 '12': 'Acre',
                 '13': 'Amazonas',
                 '14': 'Roraima',
                 '15': 'Pará',
                 '16': 'Amapá',
                 '17': 'Tocantins',
                 '21': 'Maranhão',
                 '22': 'Piauí',
                 '23': 'Ceará',
                 '24': 'Rio Grande do Norte',
                 '25': 'Paraíba',
                 '26': 'Pernambuco',
                 '27': 'Alagoas',
                 '28': 'Sergipe',
                 '29': 'Bahia',
                 '31': 'Minas Gerais',
                 '32': 'Espírito Santo',
                 '33': 'Rio de Janeiro',
                 '35': 'São Paulo',
                 '41': 'Paraná',
                 '42': 'Santa Catarina',
                 '43': 'Rio Grande do Sul',
                 '50': 'Mato Grosso do Sul',
                 '51': 'Mato Grosso',
                 '52': 'Goiás',
                 '53': 'Distrito Federal',
                 'RegN': 'Regional Norte',
                 'RegC': 'Regional Centro',
                 'RegL': 'Regional Leste',
                 'RegS': 'Regional Sul',
                 'BR': 'Brasil'}


def radar_factory(num_vars, frame='circle'):
    """Create a radar chart with `num_vars` axes."""
    # calculate evenly-spaced axis angles
    theta = 2 * np.pi * np.linspace(0, 1 - 1. / num_vars, num_vars)
    # rotate theta such that the first axis is at the top
    theta += np.pi / 2

    def draw_poly_frame(self, x0, y0, r):
        # TODO: use transforms to convert (x, y) to (r, theta)
        verts = [(r * np.cos(t) + x0, r * np.sin(t) + y0) for t in theta]
        return plt.Polygon(verts, closed=True, edgecolor='k')

    def draw_circle_frame(self, x0, y0, r):
        return plt.Circle((x0, y0), r)

    frame_dict = {'polygon': draw_poly_frame, 'circle': draw_circle_frame}
    if frame not in frame_dict:
        raise ValueError('unknown value for `frame`: %s' % frame)

    class RadarAxes(PolarAxes):
        """Class for creating a radar chart (a.k.a. a spider or star chart)

        http://en.wikipedia.org/wiki/Radar_chart
        """
        name = 'radar'
        # use 1 line segment to connect specified points
        RESOLUTION = 1
        # define draw_frame method
        draw_frame = frame_dict[frame]

        def fill(self, *args, **kwargs):
            """Override fill so that line is closed by default"""
            closed = kwargs.pop('closed', True)
            return super(RadarAxes, self).fill(closed=closed, *args, **kwargs)

        def plot(self, *args, **kwargs):
            """Override plot so that line is closed by default"""
            lines = super(RadarAxes, self).plot(*args, **kwargs)
            for line in lines:
                self._close_line(line)

        def _close_line(self, line):
            x, y = line.get_data()
            # FIXME: markers at x[0], y[0] get doubled-up
            if x[0] != x[-1]:
                x = np.concatenate((x, [x[0]]))
                y = np.concatenate((y, [y[0]]))
                line.set_data(x, y)

        def set_varlabels(self, labels, **kwargs):
            self.set_thetagrids(theta * 180 / np.pi, labels, **kwargs)

        def _gen_axes_patch(self):
            x0, y0 = (0.5, 0.5)
            r = 0.5
            return self.draw_frame(x0, y0, r)

    register_projection(RadarAxes)
    return theta


if __name__ == '__main__':

    df = pd.read_csv('../clean_data/clean_data_epiweek-weekly.csv', low_memory=False)[
        ['epiweek', 'epiyear', 'UF', 'sexo', 'POSITIVE_CASES', 'FLU_A', 'FLU_B', 'VSR', 'OTHERS', 'SRAG', 'NEGATIVE',
         'INCONCLUSIVE', 'TESTING_IGNORED', 'NOTTESTED']]

    tgt_cols = {'Agentes infecciosos detectados': ['FLU_A', 'FLU_B', 'VSR', 'OTHERS'],
                'Exames laboratoriais': ['POSITIVE_CASES', 'NEGATIVE', 'INCONCLUSIVE',
                                         'TESTING_IGNORED', 'NOTTESTED']}
    print(len(tgt_cols['Agentes infecciosos detectados']))
    spoke_labels = {'Agentes infecciosos detectados': ['Flu A', 'Flu B', 'VSR', 'Outros'],
                    'Exames laboratoriais': ['Positivo', 'Negativo', 'Inconclusivo', 'Não informado', 'Não testado']}
    sex_lbl = {'M': 'Homens', 'F': 'Mulheres', 'Total': 'Total'}
    sex_row = {'M': 1, 'F': 2, 'Total': 0}

    for uf in df.UF.unique():
        uf_sigla = tabela_ufnome[str(uf).replace('.0','')]
        for year in df.epiyear.unique():
            ########### Yearly #################

            #### Prepare figure:
            radial_grid = [.2, .4, .6, .8, 1]
            radial_lbls = ['%.0f%%' %(100*s) for s in radial_grid]
            color = 'orange'
            fig = plt.figure(figsize=(15, 15))
            # adjust spacing around the subplots
            fig.subplots_adjust(wspace=0.01, hspace=0.42, top=0.875, bottom=0.01)
            title_list = ['Agentes infecciosos detectados', 'Exames laboratoriais']

            #### Prepare data:
            for sex in ['Total', 'M', 'F']:
                b = df.loc[(df.UF == uf) & (df.epiyear == year) & (df.sexo == sex), ]
                yearly_total = b.sum()
                if (yearly_total['POSITIVE_CASES'] > 0):
                    yearly_total[tgt_cols['Agentes infecciosos detectados']] /= yearly_total['POSITIVE_CASES']
                if (yearly_total['SRAG'] > 0):
                    yearly_total[tgt_cols['Exames laboratoriais']] /= yearly_total['SRAG']

                title_txt = {'Agentes infecciosos detectados': ('Agentes infecciosos detectados - %s\n' % sex_lbl[sex] +
                            'Total de exames positivos: %.0f' % (yearly_total['POSITIVE_CASES']*yearly_total['SRAG'])),
                             'Exames laboratoriais': ('Exames laboratoriais - %s\n' % sex_lbl[sex] +
                                                      'Total de casos: %.0f' % yearly_total['SRAG'])}
                # If you don't care about the order, you can loop over data_dict.items()
                for n, title in enumerate(title_list):
                    theta = radar_factory(len(tgt_cols[title]), frame = 'circle')
                    ax = fig.add_subplot(3, 2, 2*sex_row[sex]+n+1, projection='radar')
                    plt.rgrids(radial_grid, radial_lbls, angle=45, ha='center', va='center', size='xx-large')
                    ax.set_title(title_txt[title], weight='bold', size='xx-large', position=(0.5, 1.2),
                                 horizontalalignment='center', verticalalignment='center')
                    ax.plot(theta, yearly_total[tgt_cols[title]], color=color)
                    ax.fill(theta, yearly_total[tgt_cols[title]], facecolor=color, alpha=0.25)
                    ax.set_varlabels(spoke_labels[title], fontsize=18)

                    ax.set_ylim(bottom=0, top=1)
            # add legend relative to top-left plot
            #plt.subplot(3, 2, 1)
            #plt.figtext(0.5, 0.8, 'Situação dos exames laboratoriais em %s\nAno: %s' % (uf_sigla, year),
            #           ha='center', color='black', weight='bold', size='large')
            ax_title = fig.get_axes()[0]
            ax.annotate('Situação dos exames laboratoriais: %s\nAno: %s' % (uf_sigla, year), (0.5, 0.99),
                        ha='center', va='top', color='black', weight='bold', size='xx-large', xycoords='figure '
                                                                                                       'fraction')
            plt.savefig('../plots/radar/radar_anual_%s_%s.svg' % (uf_sigla,year), bbox_inches='tight')
            plt.close()