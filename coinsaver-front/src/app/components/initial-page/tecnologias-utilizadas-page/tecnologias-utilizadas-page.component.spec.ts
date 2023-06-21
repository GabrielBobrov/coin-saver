import { ComponentFixture, TestBed } from '@angular/core/testing';

import { TecnologiasUtilizadasPageComponent } from './tecnologias-utilizadas-page.component';

describe('TecnologiasUtilizadasPageComponent', () => {
  let component: TecnologiasUtilizadasPageComponent;
  let fixture: ComponentFixture<TecnologiasUtilizadasPageComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      declarations: [ TecnologiasUtilizadasPageComponent ]
    })
    .compileComponents();

    fixture = TestBed.createComponent(TecnologiasUtilizadasPageComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
